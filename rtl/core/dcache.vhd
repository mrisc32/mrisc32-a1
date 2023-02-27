----------------------------------------------------------------------------------------------------
-- Copyright (c) 2020 Marcus Geelnard
--
-- This software is provided 'as-is', without any express or implied warranty. In no event will the
-- authors be held liable for any damages arising from the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose, including commercial
-- applications, and to alter it and redistribute it freely, subject to the following restrictions:
--
--  1. The origin of this software must not be misrepresented; you must not claim that you wrote
--     the original software. If you use this software in a product, an acknowledgment in the
--     product documentation would be appreciated but is not required.
--
--  2. Altered source versions must be plainly marked as such, and must not be misrepresented as
--     being the original software.
--
--  3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.config.all;

entity dcache is
  generic(
    CONFIG : T_CORE_CONFIG
  );
  port(
    -- Control signals.
    i_clk : in std_logic;
    i_rst : in std_logic;

    -- Data interface (slave).
    i_data_req : in std_logic;
    i_data_adr : in std_logic_vector(C_WORD_SIZE-1 downto 2);
    i_data_dat : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_data_we : in std_logic;
    i_data_sel : in std_logic_vector(C_WORD_SIZE/8-1 downto 0);
    o_data_dat : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_data_ack : out std_logic;
    o_data_busy : out std_logic;

    -- Memory interface (WB master).
    o_mem_cyc : out std_logic;
    o_mem_stb : out std_logic;
    o_mem_adr : out std_logic_vector(C_WORD_SIZE-1 downto 2);
    o_mem_dat : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_mem_we : out std_logic;
    o_mem_sel : out std_logic_vector(C_WORD_SIZE/8-1 downto 0);
    i_mem_dat : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_mem_ack : in std_logic;
    i_mem_stall : in std_logic;
    i_mem_err : in std_logic
  );
end dcache;

architecture rtl of dcache is
  constant C_REQ_FIFO_DEPTH : integer := 32;
  constant C_REQ_FIFO_WIDTH : integer := 1 + C_WORD_SIZE-2;
  constant C_LOG2_CACHE_SIZE : integer := 2;
  constant C_CACHE_SIZE : integer := 2**C_LOG2_CACHE_SIZE;

  signal s_mem_req : std_logic;
  signal s_start_req : std_logic;
  signal s_ignore_ack : std_logic;
  signal s_immediate_ack : std_logic;

  signal s_fifo_wr_en : std_logic;
  signal s_fifo_wr_data : std_logic_vector(C_REQ_FIFO_WIDTH-1 downto 0);
  signal s_fifo_full : std_logic;
  signal s_fifo_rd_en : std_logic;
  signal s_fifo_rd_data : std_logic_vector(C_REQ_FIFO_WIDTH-1 downto 0);
  signal s_fifo_empty : std_logic;

  subtype T_CACHE_TAG is std_logic_vector(C_WORD_SIZE-1 downto 2+C_LOG2_CACHE_SIZE);
  subtype T_CACHE_IDX is integer range 0 to C_CACHE_SIZE-1;

  type T_CACHE_DAT_MEM is array (0 to C_CACHE_SIZE-1) of std_logic_vector(C_WORD_SIZE-1 downto 0);
  type T_CACHE_TAG_MEM is array (0 to C_CACHE_SIZE-1) of T_CACHE_TAG;
  signal s_cache_dat_mem : T_CACHE_DAT_MEM;
  signal s_cache_tag_mem : T_CACHE_TAG_MEM;
  signal s_cache_invalidating : std_logic;
  signal s_cache_invalide_idx : T_CACHE_IDX;
  signal s_cache_got_read_hit : std_logic;
  signal s_cache_got_write_hit : std_logic;
  signal s_cache_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_cache_ack : std_logic;

  -- Ensure that the cache is using registers, not BRAM.
  attribute RAMSTYLE : string;
  attribute RAMSTYLE of s_cache_dat_mem : signal is "MLAB";  -- Intel/Altera
  attribute RAMSTYLE of s_cache_tag_mem : signal is "MLAB";
  attribute RAM_STYLE : string;
  attribute RAM_STYLE of s_cache_dat_mem : signal is "distributed";  -- Xilinx
  attribute RAM_STYLE of s_cache_tag_mem : signal is "distributed";

  function adr_to_tag(adr : std_logic_vector(C_WORD_SIZE-1 downto 2)) return T_CACHE_TAG is
  begin
    return adr(C_WORD_SIZE-1 downto 2+C_LOG2_CACHE_SIZE);
  end function;

  function adr_to_idx(adr : std_logic_vector(C_WORD_SIZE-1 downto 2)) return T_CACHE_IDX is
    variable v_idx_vec : std_logic_vector(C_LOG2_CACHE_SIZE-1 downto 0);
  begin
    v_idx_vec := adr(2+C_LOG2_CACHE_SIZE-1 downto 2);
    return to_integer(unsigned(v_idx_vec));
  end function;

  function is_cacheable(adr : std_logic_vector(C_WORD_SIZE-1 downto 2)) return boolean is
  begin
    -- We do not cache data from addresses in the range C0000000 to FFFFFFFF. That range
    -- is reserved for non-cacheable data, such as MMIO.
    return adr(C_WORD_SIZE-1 downto C_WORD_SIZE-2) /= "11";
  end function;
begin
  -- TODO(m): Implement a proper write-through cache to speed up read operations.

  -- We use a FIFO to keep track of ongoing requests.
  req_fifo: entity work.fifo
    generic map (
      G_WIDTH => C_REQ_FIFO_WIDTH,
      G_DEPTH => C_REQ_FIFO_DEPTH
    )
    port map (
      i_rst => i_rst,
      i_clk => i_clk,
      i_wr_en => s_fifo_wr_en,
      i_wr_data => s_fifo_wr_data,
      o_full => s_fifo_full,
      i_rd_en => s_fifo_rd_en,
      o_rd_data => s_fifo_rd_data,
      o_empty => s_fifo_empty
    );

  -- Shall we send a new request?
  s_mem_req <= i_data_req and (not s_cache_got_read_hit);
  s_start_req <= s_mem_req and (not i_mem_stall) and (not s_fifo_full);

  -- Write requests are ACKed immediately (on the next cycle) whenever possible.
  process (i_rst, i_clk) is
  begin
    if i_rst = '1' then
      s_immediate_ack <= '0';
    elsif rising_edge(i_clk) then
      s_immediate_ack <= s_start_req and i_data_we;
    end if;
  end process;

  -- We ignore ACKs from the Wishbone interface that have already been ACKed.
  s_ignore_ack <= s_fifo_rd_data(C_REQ_FIFO_WIDTH-1);

  -- Queue memory requests in the FIFO. The purpose is to match ACKs from the Wishbone bus with
  -- the requests that we have sent.
  s_fifo_wr_en <= s_start_req;
  s_fifo_wr_data <= i_data_we & i_data_adr;

  -- Read from the request FIFO when we get an ACK from the Wishbone bus.
  s_fifo_rd_en <= i_mem_ack and (not s_fifo_empty);

  -- We just forward all requests to the main memory interface.
  o_mem_cyc <= s_mem_req or (not s_fifo_empty);
  o_mem_stb <= s_mem_req and (not s_fifo_full);
  o_mem_adr <= i_data_adr;
  o_mem_dat <= i_data_dat;
  o_mem_we <= i_data_we;
  o_mem_sel <= i_data_sel;

  -- ...send the result back.
  -- Note: s_immediate_ack and a non-ignored i_mem_ack SHOULD never happen at the same time.
  -- If they did, one of those ACKs would be lost, which would be bad.
  o_data_ack <= s_immediate_ack or (i_mem_ack and (not s_ignore_ack)) or s_cache_ack;
  o_data_busy <= i_mem_stall or s_fifo_full;
  o_data_dat <= s_cache_dat when s_cache_ack = '1' else i_mem_dat;


  --------------------------------------------------------------------------------------------------
  -- Cache logic
  --------------------------------------------------------------------------------------------------

  -- Cache lookup.
  process (ALL) is
    variable v_hit : std_logic;
  begin
    if adr_to_tag(i_data_adr) = s_cache_tag_mem(adr_to_idx(i_data_adr)) then
      v_hit := '1';
    else
      v_hit := '0';
    end if;
    s_cache_got_read_hit <= v_hit and i_data_req and (not i_data_we);
    s_cache_got_write_hit <= v_hit and i_data_req and i_data_we;
  end process;

  process (i_rst, i_clk) is
  begin
    if i_rst = '1' then
      s_cache_dat <= (others => '0');
      s_cache_ack <= '0';
    elsif rising_edge(i_clk) then
      s_cache_dat <= s_cache_dat_mem(adr_to_idx(i_data_adr));
      s_cache_ack <= s_cache_got_read_hit;
    end if;
  end process;


  -- Cache update.
  process (i_rst, i_clk) is
    variable v_mem_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
    variable v_idx : T_CACHE_IDX;
  begin
    if i_rst = '1' then
      s_cache_invalidating <= '1';
      s_cache_invalide_idx <= 0;
    elsif rising_edge(i_clk) then
      if s_cache_invalidating = '1' then
        -- Invalidate one entry of the cache.
        -- Note: "1111..11" is an uncacheable address, so it marks the cache entry as "invalid".
        s_cache_tag_mem(s_cache_invalide_idx) <= (others => '1');

        -- Continue invalidating until we're done.
        if s_cache_invalide_idx = C_CACHE_SIZE-1 then
          s_cache_invalidating <= '0';
          s_cache_invalide_idx <= 0;
        else
          s_cache_invalide_idx <= s_cache_invalide_idx + 1;
        end if;
      elsif s_cache_got_write_hit = '1' then
        -- Update the cache entry that is being written to.
        v_mem_adr := i_data_adr;
        v_idx := adr_to_idx(v_mem_adr);
        if is_cacheable(v_mem_adr) and i_data_sel = "1111" then
          s_cache_tag_mem(v_idx) <= adr_to_tag(v_mem_adr);
          s_cache_dat_mem(v_idx) <= i_data_dat;
        else
          -- Invalidate.
          s_cache_tag_mem(v_idx) <= (others => '1');
        end if;
      elsif i_mem_ack = '1' and s_ignore_ack = '0' then
        -- Cache data that has been read from the memory.
        v_mem_adr := s_fifo_rd_data(C_WORD_SIZE-3 downto 0);
        v_idx := adr_to_idx(v_mem_adr);
        if is_cacheable(v_mem_adr) then
          s_cache_tag_mem(v_idx) <= adr_to_tag(v_mem_adr);
          s_cache_dat_mem(v_idx) <= i_mem_dat;
        end if;
      end if;
    end if;
  end process;

end rtl;
