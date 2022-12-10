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
  signal s_waiting_for_ack : std_logic;
  signal s_waiting_for_write_ack : std_logic;
  signal s_immediate_ack : std_logic;
  signal s_busy_w_write : std_logic;
  signal s_mem_cyc : std_logic;
  signal s_mem_stb : std_logic;
begin
  -- TODO(m): Implement a Store Buffer.
  --   "A store buffer is a hardware structure closer to the memory hierarchy and "buffers" up the
  --    write traffic (stores) from the processor so that the Write-back stage of the processor is
  --    complete as soon as possible."
  --   "A store buffer is a mechanism that exists in many current processors to accomplish one or
  --    more of the following: store access ordering, latency hiding and data forwarding."
  --
  -- The store buffer would be a small FIFO queue:
  --   - Write requests from the core are written to the FIFO queue, if:
  --     a) A memory read/write request is ongoing, or...
  --     b) ...the FIFO is not empty (covered by a)?).
  --   - ...otherwise the request is forwarded directly to the memory.
  --   - When the FIFO is not empty, write requests are sent to memory from the FIFO queue.
  --   - When the FIFO is full, write requests from the CPU are stalled, otherwise write requests
  --     are ACK:ed immediately.
  --   - Read requests must be stalled until the write queue is empty.
  --   - ...unless the read request can be satisified by write entries in the queue (or the
  --     currently ongoing write request to the memory).


  -- Keep track of ongoing requests (necessary for the CYC & STB signals).
  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_waiting_for_ack <= '0';
      s_waiting_for_write_ack <= '0';
      s_immediate_ack <= '0';
    elsif rising_edge(i_clk) then
      if s_waiting_for_ack = '0' or i_mem_ack = '1' then
        if i_data_req = '1' and i_mem_stall = '0' then
          -- Start a new request.
          s_waiting_for_ack <= '1';
          s_waiting_for_write_ack <= i_data_we;
          s_immediate_ack <= i_data_we;
        else
          -- End of request, and don't start a new request.
          s_waiting_for_ack <= '0';
          s_waiting_for_write_ack <= '0';
          s_immediate_ack <= '0';
        end if;
      else
        s_immediate_ack <= '0';
      end if;
    end if;
  end process;

  -- Are we servicing a write request "in the background"?
  s_busy_w_write <= (s_waiting_for_write_ack and not i_mem_ack);

  s_mem_cyc <= i_data_req or s_waiting_for_ack;
  s_mem_stb <= i_data_req and not s_busy_w_write;

  -- We just forward all requests to the main memory interface.
  o_mem_cyc <= s_mem_cyc;
  o_mem_stb <= s_mem_stb;
  o_mem_adr <= i_data_adr;
  o_mem_dat <= i_data_dat;
  o_mem_we <= i_data_we;
  o_mem_sel <= i_data_sel;

  -- ...send the result back.
  o_data_dat <= i_mem_dat;
  o_data_ack <= s_immediate_ack or (i_mem_ack and not s_waiting_for_write_ack);
  o_data_busy <= i_mem_stall or s_busy_w_write;
end rtl;
