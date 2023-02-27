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
  constant C_REQ_FIFO_DEPTH : integer := 32;
  constant C_REQ_FIFO_WIDTH : integer := 1;

  signal s_start_req : std_logic;
  signal s_ignore_ack : std_logic;
  signal s_immediate_ack : std_logic;

  signal s_fifo_wr_en : std_logic;
  signal s_fifo_wr_data : std_logic_vector(C_REQ_FIFO_WIDTH-1 downto 0);
  signal s_fifo_full : std_logic;
  signal s_fifo_rd_en : std_logic;
  signal s_fifo_rd_data : std_logic_vector(C_REQ_FIFO_WIDTH-1 downto 0);
  signal s_fifo_empty : std_logic;
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
  s_start_req <= i_data_req and (not i_mem_stall) and (not s_fifo_full);

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
  s_ignore_ack <= s_fifo_rd_data(0);

  -- Queue memory requests in the FIFO. The purpose is to match ACKs from the Wishbone bus with
  -- the requests that we have sent.
  s_fifo_wr_en <= s_start_req;
  s_fifo_wr_data(0) <= i_data_we;

  -- Read from the request FIFO when we get an ACK from the Wishbone bus.
  s_fifo_rd_en <= i_mem_ack and (not s_fifo_empty);

  -- We just forward all requests to the main memory interface.
  o_mem_cyc <= i_data_req or (not s_fifo_empty);
  o_mem_stb <= i_data_req and (not s_fifo_full);
  o_mem_adr <= i_data_adr;
  o_mem_dat <= i_data_dat;
  o_mem_we <= i_data_we;
  o_mem_sel <= i_data_sel;

  -- ...send the result back.
  -- Note: s_immediate_ack and a non-ignored i_mem_ack SHOULD never happen at the same time.
  -- If they did, one of those ACKs would be lost, which would be bad.
  o_data_ack <= s_immediate_ack or (i_mem_ack and (not s_ignore_ack));
  o_data_busy <= i_mem_stall or s_fifo_full;
  o_data_dat <= i_mem_dat;
end rtl;
