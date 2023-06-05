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
    i_invalidate : in std_logic;
    i_flush : in std_logic;

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
  --------------------------------------------------------------------------------------------------
  -- Cache memory configuration & signals.
  --------------------------------------------------------------------------------------------------

  constant C_LOG2_LINES : integer := 10;
  constant C_LOG2_WORDS_PER_LINE : integer := 3;
  constant C_LINES : integer := 2**C_LOG2_LINES;
  constant C_WORDS_PER_LINE : integer := 2**C_LOG2_WORDS_PER_LINE;

  constant C_TAG_WIDTH : integer := 1 + (30 - C_LOG2_LINES - C_LOG2_WORDS_PER_LINE);
  constant C_TAG_ADDR_BITS : integer := C_LOG2_LINES;
  constant C_MAX_TAG_ADDR : unsigned := to_unsigned((2 ** C_TAG_ADDR_BITS) - 1, C_TAG_ADDR_BITS);
  constant C_DATA_ADDR_BITS : integer := C_LOG2_LINES + C_LOG2_WORDS_PER_LINE;

  signal s_tagmem_wr_data : std_logic_vector(C_TAG_WIDTH-1 downto 0);
  signal s_tagmem_wr_addr : std_logic_vector(C_TAG_ADDR_BITS-1 downto 0);
  signal s_tagmem_wr_en : std_logic;
  signal s_tagmem_rd_addr : std_logic_vector(C_TAG_ADDR_BITS-1 downto 0);
  signal s_tagmem_rd_data : std_logic_vector(C_TAG_WIDTH-1 downto 0);
  signal s_forwarded_tagmem_data : std_logic_vector(C_TAG_WIDTH-1 downto 0);
  signal s_use_forwarded_tagmem_data : std_logic;
  signal s_tagmem_rd_data_actual : std_logic_vector(C_TAG_WIDTH-1 downto 0);

  signal s_datamem_wr_data : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_datamem_wr_addr : std_logic_vector(C_DATA_ADDR_BITS-1 downto 0);
  signal s_datamem_wr_en : std_logic;
  signal s_datamem_rd_addr : std_logic_vector(C_DATA_ADDR_BITS-1 downto 0);
  signal s_datamem_rd_data : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_forwarded_datamem_data : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_use_forwarded_datamem_data : std_logic;
  signal s_datamem_rd_data_actual : std_logic_vector(C_WORD_SIZE-1 downto 0);

  --------------------------------------------------------------------------------------------------
  -- Cache lookup signals.
  --------------------------------------------------------------------------------------------------

  signal s_stall_dc1 : std_logic;
  signal s_dc1_data_req : std_logic;
  signal s_dc1_data_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_dc1_data_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_dc1_data_we : std_logic;
  signal s_dc1_data_sel : std_logic_vector(C_WORD_SIZE/8-1 downto 0);
  signal s_dc1_cacheable : std_logic;

  --------------------------------------------------------------------------------------------------
  -- Cache state machine.
  --------------------------------------------------------------------------------------------------

  -- State machine states.
  type T_DC_STATE is (
    RESET,
    INVALIDATE,
    FLUSH,
    READY,
    WAIT_FOR_MEM_READY,
    UPDATE_CACHE_LINE,
    WAIT_FOR_UNCACHED_READ
  );

  signal s_state : T_DC_STATE;
  signal s_next_state : T_DC_STATE;

  signal s_invalidate_requested : std_logic;
  signal s_invalidate_addr : unsigned(C_TAG_ADDR_BITS-1 downto 0);
  signal s_next_invalidate_addr : unsigned(C_TAG_ADDR_BITS-1 downto 0);
  signal s_flush_requested : std_logic;

  signal s_word_idx : std_logic_vector(C_LOG2_WORDS_PER_LINE-1 downto 0);
  signal s_next_word_idx : std_logic_vector(C_LOG2_WORDS_PER_LINE-1 downto 0);
  signal s_last_word_idx : std_logic_vector(C_LOG2_WORDS_PER_LINE-1 downto 0);

  signal s_cache_line_done : std_logic;
  signal s_next_cache_line_done : std_logic;

  signal s_repeat_request : std_logic;
  signal s_next_repeat_request : std_logic;

  --------------------------------------------------------------------------------------------------
  -- Pending requests FIFO configuration & signals.
  --------------------------------------------------------------------------------------------------

  constant C_REQ_FIFO_DEPTH : integer := 32;
  constant C_REQ_FIFO_WIDTH : integer := C_LOG2_WORDS_PER_LINE + 1;  -- word_idx + we

  signal s_fifo_wr_en : std_logic;
  signal s_fifo_wr_data : std_logic_vector(C_REQ_FIFO_WIDTH-1 downto 0);
  signal s_fifo_full : std_logic;
  signal s_fifo_rd_en : std_logic;
  signal s_fifo_rd_data : std_logic_vector(C_REQ_FIFO_WIDTH-1 downto 0);
  signal s_fifo_empty : std_logic;

  --------------------------------------------------------------------------------------------------
  -- Memory request signals.
  --------------------------------------------------------------------------------------------------

  signal s_can_start_req : std_logic;
  signal s_start_req : std_logic;

  signal s_req_en : std_logic;
  signal s_req_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_req_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_req_we : std_logic;
  signal s_req_sel : std_logic_vector(C_WORD_SIZE/8-1 downto 0);

  signal s_resp_we : std_logic;
  signal s_resp_ack : std_logic;
  signal s_resp_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_resp_word_idx : std_logic_vector(C_LOG2_WORDS_PER_LINE-1 downto 0);

  --------------------------------------------------------------------------------------------------
  -- Helper functions.
  --------------------------------------------------------------------------------------------------

  function is_cacheable(addr : std_logic_vector) return std_logic is
  begin
    -- We hardcode addresses with the two most significant bits set as non-cacheable. Thus the
    -- address range C0000000-FFFFFFFF can be used for memory mapped I/O and similar.
    if addr(addr'left downto addr'left-1) = "11" then
      return '0';
    end if;

    -- All other address ranges are cacheable.
    return '1';
  end function;

  function make_valid_tag(addr : std_logic_vector) return std_logic_vector is
  begin
    return "1" & addr(addr'left downto addr'left - (C_TAG_WIDTH-1) + 1);
  end function;

  function make_invalid_tag return std_logic_vector is
  begin
    return "0" & std_logic_vector(to_unsigned(0, C_TAG_WIDTH-1));
  end function;

  function make_tagmem_addr(addr : std_logic_vector) return std_logic_vector is
  begin
    return addr(addr'right + (C_LOG2_LINES + C_LOG2_WORDS_PER_LINE) - 1 downto addr'right + C_LOG2_WORDS_PER_LINE);
  end function;

  function make_datamem_idx_addr(addr : std_logic_vector; idx : std_logic_vector) return std_logic_vector is
  begin
    return make_tagmem_addr(addr) & idx(idx'right + C_LOG2_WORDS_PER_LINE - 1 downto idx'right);
  end function;

  function make_datamem_addr(addr : std_logic_vector) return std_logic_vector is
  begin
    return make_datamem_idx_addr(addr, addr);
  end function;

  function mix_words(sel : std_logic_vector; old_data : std_logic_vector; new_data : std_logic_vector) return std_logic_vector is
    variable v_data : std_logic_vector(C_WORD_SIZE-1 downto 0);
    variable v_idx : integer;
  begin
    for k in sel'range loop
      v_idx := 8 * k;
      if sel(k) = '1' then
        v_data(v_idx+7 downto v_idx) := new_data(v_idx+7 downto v_idx);
      else
        v_data(v_idx+7 downto v_idx) := old_data(v_idx+7 downto v_idx);
      end if;
    end loop;
    return v_data;
  end function;
begin
  --------------------------------------------------------------------------------------------------
  -- Cache memory instantiation.
  --------------------------------------------------------------------------------------------------

  -- Tag memory.
  tag_ram_1: entity work.ram_dual_port
    generic map (
      WIDTH => C_TAG_WIDTH,
      ADDR_BITS => C_TAG_ADDR_BITS,
      PREFER_DISTRIBUTED => false
    )
    port map (
      i_clk => i_clk,
      i_write_data => s_tagmem_wr_data,
      i_write_addr => s_tagmem_wr_addr,
      i_we => s_tagmem_wr_en,
      i_read_addr => s_tagmem_rd_addr,
      o_read_data => s_tagmem_rd_data
    );

  -- Data memory.
  data_ram_1: entity work.ram_dual_port
    generic map (
      WIDTH => C_WORD_SIZE,
      ADDR_BITS => C_DATA_ADDR_BITS,
      PREFER_DISTRIBUTED => false
    )
    port map (
      i_clk => i_clk,
      i_write_data => s_datamem_wr_data,
      i_write_addr => s_datamem_wr_addr,
      i_we => s_datamem_wr_en,
      i_read_addr => s_datamem_rd_addr,
      o_read_data => s_datamem_rd_data
    );

  -- Handle forwarding of last written values.
  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_forwarded_tagmem_data <= (others => '0');
      s_use_forwarded_tagmem_data <= '0';
      s_forwarded_datamem_data <= (others => '0');
      s_use_forwarded_datamem_data <= '0';
    elsif rising_edge(i_clk) then
      -- Forward data that is written to tagmem?
      if s_tagmem_wr_en = '1' and s_tagmem_wr_addr = s_tagmem_rd_addr then
        s_use_forwarded_tagmem_data <= '1';
      else
        s_use_forwarded_tagmem_data <= '0';
      end if;
      s_forwarded_tagmem_data <= s_tagmem_wr_data;

      -- Forward data that is written to datamem?
      if s_datamem_wr_en = '1' and s_datamem_wr_addr = s_datamem_rd_addr then
        s_use_forwarded_datamem_data <= '1';
      else
        s_use_forwarded_datamem_data <= '0';
      end if;
      s_forwarded_datamem_data <= s_datamem_wr_data;
    end if;
  end process;

  s_tagmem_rd_data_actual <= s_forwarded_tagmem_data when s_use_forwarded_tagmem_data = '1' else s_tagmem_rd_data;
  s_datamem_rd_data_actual <= s_forwarded_datamem_data when s_use_forwarded_datamem_data = '1' else s_datamem_rd_data;


  --------------------------------------------------------------------------------------------------
  -- Cache lookup (pipelined, stages: DC1, DC2).
  --------------------------------------------------------------------------------------------------

  -- DC1: Send a read request to the cache.
  s_tagmem_rd_addr <= make_tagmem_addr(i_data_adr);
  s_datamem_rd_addr <= make_datamem_addr(i_data_adr);

  -- Outputs to the DC2 stage.
  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_dc1_data_req <= '0';
      s_dc1_data_adr <= (others => '0');
      s_dc1_data_dat <= (others => '0');
      s_dc1_data_we <= '0';
      s_dc1_data_sel <= (others => '0');
      s_dc1_cacheable <= '0';
    elsif rising_edge(i_clk) then
      if s_stall_dc1 = '0' then
        s_dc1_data_req <= i_data_req;
        s_dc1_data_adr <= i_data_adr;
        s_dc1_data_dat <= i_data_dat;
        s_dc1_data_we <= i_data_we;
        s_dc1_data_sel <= i_data_sel;
        s_dc1_cacheable <= is_cacheable(i_data_adr);
      end if;
    end if;
  end process;

  -- Do we need to stall any new cache requests?
  s_stall_dc1 <= '0' when s_next_state = READY and s_next_repeat_request = '0' else '1';

  -- Can we accept new requests from the data interface?
  o_data_busy <= s_stall_dc1;


  --------------------------------------------------------------------------------------------------
  -- Cache line update state machine.
  --------------------------------------------------------------------------------------------------

  -- Define the memory request parameters.
  s_req_adr <= s_dc1_data_adr(C_WORD_SIZE-1 downto C_LOG2_WORDS_PER_LINE+2) & s_word_idx;
  s_req_dat <= s_dc1_data_dat;
  s_req_sel <= s_dc1_data_sel;

  -- Should we repeat the current request during the next cycle?
  s_next_repeat_request <= s_start_req and i_mem_stall;

  process(ALL)
  begin
    -- Default values. Optionally overridden in different states.
    s_next_state <= s_state;
    s_next_invalidate_addr <= (others => '-');
    s_next_cache_line_done <= s_cache_line_done;

    s_tagmem_wr_data <= (others => '-');
    s_tagmem_wr_addr <= (others => '-');
    s_tagmem_wr_en <= '0';
    s_datamem_wr_data <= (others => '-');
    s_datamem_wr_addr <= (others => '-');
    s_datamem_wr_en <= '0';

    s_req_en <= '0';
    s_req_we <= '0';
    o_data_ack <= '0';
    o_data_dat <= (others => '-');

    -- Default word_idx.
    if s_stall_dc1 = '0' then
      s_next_word_idx <= i_data_adr(C_LOG2_WORDS_PER_LINE + 2 - 1 downto 2);
    else
      s_next_word_idx <= s_dc1_data_adr(C_LOG2_WORDS_PER_LINE + 2 - 1 downto 2);
    end if;

    case s_state is
      when RESET =>
        s_next_invalidate_addr <= (others => '0');
        s_next_state <= INVALIDATE;

      when INVALIDATE =>
        s_next_invalidate_addr <= s_invalidate_addr + 1;
        s_tagmem_wr_data <= make_invalid_tag;
        s_tagmem_wr_addr <= std_logic_vector(s_invalidate_addr);
        s_tagmem_wr_en <= '1';

        if s_invalidate_addr = C_MAX_TAG_ADDR then
          s_next_state <= READY;
        end if;

      when FLUSH =>
        -- Since this is a write-through cache, we have nothing to do here.
        s_next_state <= READY;

      when READY =>
        s_next_cache_line_done <= '0';

        if s_repeat_request = '1' then
          -- Repeat the previous request.
          -- Note: it's OK for the FIFO to be full as we won't add a new entry.
          s_req_en <= '1';
          s_req_we <= s_dc1_data_we;  -- TODO(m): This can only ever be '1', right?
        elsif s_flush_requested = '1' then
          -- TODO(m): Flush and Invalidate should be performed in the order of their requests.
          s_next_state <= FLUSH;
        elsif s_invalidate_requested = '1' then
          s_next_invalidate_addr <= (others => '0');
          s_next_state <= INVALIDATE;
        elsif s_dc1_data_req = '1' then
          -- ACK writes immediately (regardless if we had a hit or when we start the memory
          -- request), as we will take care of the write operation somehow down the line, and we
          -- won't ACK the request then.
          if s_dc1_data_we = '1' then
            o_data_ack <= '1';
          end if;

          -- Check if we had a cache hit or miss.
          if s_tagmem_rd_data_actual = make_valid_tag(s_dc1_data_adr) then
            if s_dc1_data_we = '0' then
              -- Cache read hit -> respond to request.
              o_data_ack <= '1';
              o_data_dat <= s_datamem_rd_data_actual;
            else
              -- Cache write hit -> update cache entry.
              s_datamem_wr_data <= mix_words(s_dc1_data_sel, s_datamem_rd_data_actual, s_dc1_data_dat);
              s_datamem_wr_addr <= make_datamem_addr(s_dc1_data_adr);
              s_datamem_wr_en <= '1';

              if s_can_start_req = '1' then
                -- Initiate a single word write operation.
                s_req_en <= '1';
                s_req_we <= '1';
              else
                s_next_state <= WAIT_FOR_MEM_READY;
              end if;
            end if;
          else
            if s_can_start_req = '1' then
              if s_dc1_data_we = '0' then
                -- Cache read miss.
                if s_dc1_cacheable = '1' then
                  -- Initiate a full cache line read cycle.
                  s_req_en <= '1';
                  s_req_we <= '0';
                  s_next_word_idx <= std_logic_vector(unsigned(s_word_idx) + 1);
                  s_next_state <= UPDATE_CACHE_LINE;
                else
                  -- Initiate a single word read operation (for uncachable memories).
                  s_req_en <= '1';
                  s_req_we <= '0';
                  s_next_state <= WAIT_FOR_UNCACHED_READ;
                end if;
              else
                -- Cache write miss.
                -- Initiate a single word write operation.
                s_req_en <= '1';
                s_req_we <= '1';
              end if;
            else
              s_next_state <= WAIT_FOR_MEM_READY;
            end if;
          end if;
        end if;

      when WAIT_FOR_MEM_READY =>
        if s_can_start_req = '1' then
          if s_dc1_data_we = '0' then
            if s_dc1_cacheable = '1' then
              -- Initiate a full cache line read cycle.
              s_req_en <= '1';
              s_req_we <= '0';
              s_next_word_idx <= std_logic_vector(unsigned(s_word_idx) + 1);
              s_next_state <= UPDATE_CACHE_LINE;
            else
              -- Initiate a single word read operation (for uncachable memories).
              s_req_en <= '1';
              s_req_we <= '0';
              s_next_state <= WAIT_FOR_UNCACHED_READ;
            end if;
          else
            -- Initiate a single word write operation.
            s_req_en <= '1';
            s_req_we <= '1';
            s_next_state <= READY;
          end if;
        end if;

      when UPDATE_CACHE_LINE =>
        -- Unless we can send a new (non-repeated) request, stick to the current word idx.
        s_next_word_idx <= s_word_idx;

        if s_repeat_request = '1' then
          s_req_en <= '1';
          s_req_we <= '0';
        elsif s_can_start_req = '1' and s_cache_line_done = '0' then
          -- Send a new read request ASAP.
          s_req_en <= '1';
          s_req_we <= '0';
          s_next_word_idx <= std_logic_vector(unsigned(s_word_idx) + 1);

          -- Final request for this cache line?
          if s_word_idx = s_last_word_idx then
            s_next_cache_line_done <= '1';
          end if;
        end if;

        -- Collect read results whenever we get an ACK back.
        if s_resp_ack = '1' then
          -- Write the read word to the correct index in the cache line.
          s_datamem_wr_data <= s_resp_dat;
          s_datamem_wr_addr <= make_datamem_idx_addr(s_dc1_data_adr, s_resp_word_idx);
          s_datamem_wr_en <= '1';

          -- Respond to the data interface when we have a matching address.
          if s_resp_word_idx = s_dc1_data_adr(C_LOG2_WORDS_PER_LINE + 2 - 1 downto 2) then
            o_data_ack <= '1';
            o_data_dat <= s_resp_dat;
          end if;

          -- Final response for this cache line?
          if s_resp_word_idx = s_last_word_idx then
            -- Mark the cache line as valid.
            s_tagmem_wr_data <= make_valid_tag(s_dc1_data_adr);
            s_tagmem_wr_addr <= make_tagmem_addr(s_dc1_data_adr);
            s_tagmem_wr_en <= '1';

            s_next_state <= READY;
          end if;
        end if;

      when WAIT_FOR_UNCACHED_READ =>
        if s_repeat_request = '1' then
          -- Repeat the request.
          -- Note: it's OK for the FIFO to be full as we won't add a new entry.
          s_req_en <= '1';
          s_req_we <= '0';
        end if;
        if s_resp_ack = '1' then
          -- Respond to request.
          o_data_ack <= '1';
          o_data_dat <= s_resp_dat;
          s_next_state <= READY;
        end if;

      when others =>
        s_next_state <= RESET;
    end case;
  end process;

  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_invalidate_requested <= '0';
      s_flush_requested <= '0';
      s_invalidate_addr <= (others => '0');
      s_word_idx <= (others => '0');
      s_last_word_idx <= (others => '0');
      s_cache_line_done <= '0';
      s_repeat_request <= '0';
      s_state <= RESET;
    elsif rising_edge(i_clk) then
      -- Sticky remember invalidate requests.
      if i_invalidate = '1' then
        s_invalidate_requested <= '1';
      elsif s_next_state = INVALIDATE then
        s_invalidate_requested <= '0';
      end if;

      -- Sticky remember flush requests.
      if i_flush = '1' then
        s_flush_requested <= '1';
      elsif s_next_state = FLUSH then
        s_flush_requested <= '0';
      end if;

      -- Update the invalidate line address.
      s_invalidate_addr <= s_next_invalidate_addr;

      -- Update the word-within-cache-line counter.
      if s_next_repeat_request = '0' then
        s_word_idx <= s_next_word_idx;
      end if;
      if s_next_state = READY then
        s_last_word_idx <= std_logic_vector(unsigned(s_next_word_idx) - 1);
      end if;

      -- Keep track of whether or not all requestes for a cache line have been sent to memory.
      if s_next_repeat_request= '0' then
        s_cache_line_done <= s_next_cache_line_done;
      end if;

      -- If we got a STALL from the WB interface during the last cycle, we need to repeat the
      -- request (if any).
      s_repeat_request <= s_next_repeat_request;

      -- Move to the next requested state.
      s_state <= s_next_state;
    end if;
  end process;


  --------------------------------------------------------------------------------------------------
  -- We use a FIFO to keep track of ongoing requests. The purpose is to match ACKs from the Wishbone
  -- bus with the requests that we have sent.
  --------------------------------------------------------------------------------------------------

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

  -- Queue memory requests in the FIFO (if we're repeating a request, it's already in the FIFO).
  s_fifo_wr_en <= s_start_req and (not s_repeat_request);
  s_fifo_wr_data <= s_req_adr(C_LOG2_WORDS_PER_LINE+2-1 downto 2) & s_req_we;

  -- Read from the request FIFO when we get an ACK from the Wishbone bus.
  s_fifo_rd_en <= i_mem_ack and (not s_fifo_empty);

  --------------------------------------------------------------------------------------------------
  -- Memory interface: Send requests.
  --------------------------------------------------------------------------------------------------

  -- Is the FIFO ready to accept new requests?
  s_can_start_req <= not s_fifo_full;

  -- Shall we send a new request?
  s_start_req <= s_req_en and (s_can_start_req or s_repeat_request);

  -- Forward all requests to the main memory interface.
  o_mem_cyc <= s_req_en or (not s_fifo_empty);
  o_mem_stb <= s_start_req;
  o_mem_adr <= s_req_adr;
  o_mem_dat <= s_req_dat;
  o_mem_we <= s_req_we;
  o_mem_sel <= s_req_sel;

  --------------------------------------------------------------------------------------------------
  -- Memory interface: Receive request responses.
  --------------------------------------------------------------------------------------------------

  -- Get information about the request that this ACK (if any) corresponds to.
  s_resp_we <= s_fifo_rd_data(0);
  s_resp_word_idx <= s_fifo_rd_data(C_LOG2_WORDS_PER_LINE downto 1);

  -- Note: We ignore ACKs from write operations, since we never wait for those ACKs.
  s_resp_ack <= i_mem_ack and (not s_resp_we);
  s_resp_dat <= i_mem_dat;
end rtl;
