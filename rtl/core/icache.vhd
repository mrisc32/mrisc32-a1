----------------------------------------------------------------------------------------------------
-- Copyright (c) 2018 Marcus Geelnard
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

entity icache is
  generic(
    CONFIG : T_CORE_CONFIG
  );
  port(
    -- Control signals.
    i_clk : in std_logic;
    i_rst : in std_logic;
    i_invalidate : in std_logic;

    -- Instruction fetch interface (slave).
    i_instr_req : in std_logic;
    i_instr_adr : in std_logic_vector(C_WORD_SIZE-1 downto 2);
    o_instr_dat : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_instr_ack : out std_logic;

    -- Memory interface (WB master).
    o_mem_cyc : out std_logic;
    o_mem_stb : out std_logic;
    o_mem_adr : out std_logic_vector(C_WORD_SIZE-1 downto 2);
    i_mem_dat : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_mem_ack : in std_logic;
    i_mem_stall : in std_logic;
    i_mem_err : in std_logic
  );
end icache;

architecture rtl of icache is
  -- Cache size configuration.
  -- TODO(m): Collect this from CONFIG.
  -- Cache data format: [TAG, DATA]
  -- Tag format:        [VALID (0/1), ADDR(31 downto N)]
  constant C_CACHE_ADDR_BITS : positive := 10;
  constant C_MAX_CACHE_ADDR : unsigned := to_unsigned((2 ** C_CACHE_ADDR_BITS) - 1, C_CACHE_ADDR_BITS);
  constant C_TAG_SIZE : positive := 1 + C_WORD_SIZE - 2 - C_CACHE_ADDR_BITS;
  constant C_CACHE_WIDTH : positive := C_TAG_SIZE + C_WORD_SIZE;

  -- State machine states.
  type T_IC_STATE is (
    RESET,
    INVALIDATE,
    READY,
    WAIT_FOR_MEM
  );

  signal s_state : T_IC_STATE;
  signal s_next_state : T_IC_STATE;

  signal s_write_addr : std_logic_vector(C_CACHE_ADDR_BITS-1 downto 0);
  signal s_write_data : std_logic_vector(C_CACHE_WIDTH-1 downto 0);
  signal s_write_en : std_logic;

  signal s_read_addr : std_logic_vector(C_CACHE_ADDR_BITS-1 downto 0);
  signal s_read_data : std_logic_vector(C_CACHE_WIDTH-1 downto 0);

  signal s_lookup_en : std_logic;
  signal s_lookup_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_cache_miss : std_logic;

  signal s_mem_stb : std_logic;
  signal s_mem_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_last_mem_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_retry_stb : std_logic;

  signal s_invalidate_requested : std_logic;
  signal s_invalidate_addr : unsigned(C_CACHE_ADDR_BITS-1 downto 0);
  signal s_next_invalidate_addr : unsigned(C_CACHE_ADDR_BITS-1 downto 0);

  function addr2cache_addr(addr : std_logic_vector) return std_logic_vector is
  begin
    return addr(C_CACHE_ADDR_BITS+2-1 downto 2);
  end function;

  function addr2tag(addr : std_logic_vector) return std_logic_vector is
  begin
    return "1" & addr(C_WORD_SIZE-1 downto C_CACHE_ADDR_BITS+2);
  end function;

  function make_cache_dat(addr : std_logic_vector; data : std_logic_vector) return std_logic_vector is
  begin
    return addr2tag(addr) & data;
  end function;

  function cache_dat2tag(cache_dat : std_logic_vector) return std_logic_vector is
    variable v_tag : std_logic_vector(C_TAG_SIZE-1 downto 0);
  begin
    v_tag := cache_dat(C_CACHE_WIDTH-1 downto C_WORD_SIZE);
    return v_tag;
  end function;

  function cache_dat2data(cache_dat : std_logic_vector) return std_logic_vector is
  begin
    return cache_dat(C_WORD_SIZE-1 downto 0);
  end function;

  function tag_cache_addr2addr(tag : std_logic_vector; cache_addr : std_logic_vector) return std_logic_vector is
  begin
    return tag(C_TAG_SIZE-2 downto 0) & cache_addr;
  end function;

  function tag_is_valid(tag : std_logic_vector) return boolean is
  begin
    return tag(C_TAG_SIZE-1) = '1';
  end function;
begin
  -----------------------------------------------------------------------------
  -- Cache RAM.
  -----------------------------------------------------------------------------

  -- TODO(m): Use a separate tag RAM and use cache lines of 4-8 words each (it
  -- should save BRAM).
  cache_ram_1: entity work.ram_dual_port
    generic map (
      WIDTH => C_CACHE_WIDTH,
      ADDR_BITS => C_CACHE_ADDR_BITS,
      PREFER_DISTRIBUTED => false
    )
    port map (
      i_clk => i_clk,
      i_write_data => s_write_data,
      i_write_addr => s_write_addr,
      i_we => s_write_en,
      i_read_addr => s_read_addr,
      o_read_data => s_read_data
    );

  -----------------------------------------------------------------------------
  -- Cache lookup.
  --
  -- TODO(m): Document me!
  -----------------------------------------------------------------------------

  -- The lower bits of the address are used for addressing the cache.
  s_read_addr <= i_instr_adr(C_CACHE_ADDR_BITS+2-1 downto 2);

  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_lookup_en <= '0';
      s_lookup_adr <= (others => '0');
    elsif rising_edge(i_clk) then
      -- Are we ready to do a new lookup?
      if s_state = READY or s_state = WAIT_FOR_MEM then
        s_lookup_en <= i_instr_req;
        if i_instr_req = '1' then
          s_lookup_adr <= i_instr_adr;
        end if;
      else
        s_lookup_en <= '0';
      end if;
    end if;
  end process;

  process(ALL)
    variable v_write_tag : std_logic_vector(C_TAG_SIZE-1 downto 0);
  begin
    -- Cache hit?
    v_write_tag := cache_dat2tag(s_write_data);
    if s_lookup_en = '1' and
       s_write_en = '1' and
       tag_is_valid(v_write_tag) and
       tag_cache_addr2addr(v_write_tag, s_write_addr) = s_lookup_adr then
      -- Bypass result from memory read.
      s_cache_miss <= '0';
      o_instr_ack <= '1';
      o_instr_dat <= cache_dat2data(s_write_data);
    elsif s_lookup_en = '1' and cache_dat2tag(s_read_data) = addr2tag(s_lookup_adr) then
      -- Cache hit.
      s_cache_miss <= '0';
      o_instr_ack <= '1';
      o_instr_dat <= cache_dat2data(s_read_data);
    else
      -- Miss (or no lookup requested).
      s_cache_miss <= s_lookup_en;
      o_instr_ack <= '0';
      o_instr_dat <= (others => '0');
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Cache miss handling.
  --
  -- TODO(m): Document me!
  -----------------------------------------------------------------------------

  process(ALL)
  begin
    case s_state is
      when RESET =>
        s_next_invalidate_addr <= (others => '0');
        s_write_addr <= (others => '0');
        s_write_data <= (others => '0');
        s_write_en <= '0';
        o_mem_cyc <= '0';
        s_mem_stb <= '0';
        s_mem_adr <= (others => '0');
        s_next_state <= INVALIDATE;

      when INVALIDATE =>
        s_next_invalidate_addr <= s_invalidate_addr + 1;
        s_write_addr <= std_logic_vector(s_invalidate_addr);
        s_write_data <= (others => '0');  -- VALID = 0
        s_write_en <= '1';
        o_mem_cyc <= '0';
        s_mem_stb <= '0';
        s_mem_adr <= (others => '0');

        if s_invalidate_addr = C_MAX_CACHE_ADDR then
          s_next_state <= READY;
        else
          s_next_state <= INVALIDATE;
        end if;

      when READY =>
        s_next_invalidate_addr <= (others => '0');
        s_write_addr <= std_logic_vector(s_invalidate_addr);
        s_write_data <= (others => '0');
        s_write_en <= '0';
        s_mem_adr <= s_lookup_adr;

        if s_invalidate_requested = '1' then
          o_mem_cyc <= '0';
          s_mem_stb <= '0';
          s_next_state <= INVALIDATE;
        elsif s_cache_miss = '1' then
          o_mem_cyc <= '1';
          s_mem_stb <= '1';
          s_next_state <= WAIT_FOR_MEM;
        else
          o_mem_cyc <= '0';
          s_mem_stb <= '0';
          s_next_state <= READY;
        end if;

      when WAIT_FOR_MEM =>
        -- TODO(m): Handle i_mem_err.
        s_next_invalidate_addr <= (others => '0');
        s_write_addr <= addr2cache_addr(s_last_mem_adr);
        s_write_data <= make_cache_dat(s_last_mem_adr, i_mem_dat);
        s_write_en <= i_mem_ack;
        o_mem_cyc <= '1';
        s_mem_adr <= s_lookup_adr;

        if i_mem_ack = '1' then
          -- Return to WAIT_FOR_MEM if we got a new miss.
          if s_cache_miss = '1' then
            s_mem_stb <= '1';
            s_next_state <= WAIT_FOR_MEM;
          else
            s_mem_stb <= '0';
            s_next_state <= READY;
          end if;
        else
          s_mem_stb <= s_retry_stb;
          s_next_state <= WAIT_FOR_MEM;
        end if;

      when others =>
        s_next_invalidate_addr <= (others => '0');
        s_write_addr <= (others => '0');
        s_write_data <= (others => '0');
        s_write_en <= '0';
        o_mem_cyc <= '0';
        s_mem_stb <= '0';
        s_mem_adr <= (others => '0');
        s_next_state <= RESET;
    end case;
  end process;

  o_mem_stb <= s_mem_stb;
  o_mem_adr <= s_mem_adr;

  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_invalidate_requested <= '0';
      s_invalidate_addr <= (others => '0');
      s_state <= RESET;
      s_last_mem_adr <= (others => '0');
      s_retry_stb <= '0';
    elsif rising_edge(i_clk) then
      if i_invalidate = '1' then
        s_invalidate_requested <= '1';
      elsif s_next_state = INVALIDATE then
        s_invalidate_requested <= '0';
      end if;
      s_invalidate_addr <= s_next_invalidate_addr;
      s_state <= s_next_state;
      if s_mem_stb = '1' then
        s_last_mem_adr <= s_mem_adr;
      end if;
      s_retry_stb <= s_mem_stb and i_mem_stall;
    end if;
  end process;
end rtl;
