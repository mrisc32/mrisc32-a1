----------------------------------------------------------------------------------------------------
-- Copyright (c) 2019 Marcus Geelnard
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

----------------------------------------------------------------------------------------------------
-- This is a single CPU core, including the pipeline and L1 caches.
--
-- The core exposes two memory interfaces: one for instruction memory and one for data memory, in
-- a Harvard architecture fashion.
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.config.all;
use work.debug.all;

entity core is
  generic(
    CONFIG : T_CORE_CONFIG := C_CORE_CONFIG_DEFAULT
  );
  port(
    -- Control signals.
    i_clk : in std_logic;
    i_rst : in std_logic;

    -- Instruction memory interface (Wishbone B4 pipelined master).
    o_imem_cyc : out std_logic;
    o_imem_stb : out std_logic;
    o_imem_adr : out std_logic_vector(C_WORD_SIZE-1 downto 2);
    i_imem_dat : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_imem_ack : in std_logic;
    i_imem_stall : in std_logic;
    i_imem_err : in std_logic;

    -- Data memory interface (Wishbone B4 pipelined master).
    o_dmem_cyc : out std_logic;
    o_dmem_stb : out std_logic;
    o_dmem_adr : out std_logic_vector(C_WORD_SIZE-1 downto 2);
    o_dmem_dat : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_dmem_we : out std_logic;
    o_dmem_sel : out std_logic_vector(C_WORD_SIZE/8-1 downto 0);
    i_dmem_dat : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_dmem_ack : in std_logic;
    i_dmem_stall : in std_logic;
    i_dmem_err : in std_logic;

    -- Debug trace interface.
    o_debug_trace : out T_DEBUG_TRACE
  );
end core;

architecture rtl of core is
  -- Cache control signals.
  signal s_invalidate_icache : std_logic;
  signal s_invalidate_dcache : std_logic;
  signal s_flush_dcache : std_logic;

  -- Pipeline instruction bus master signals.
  signal s_instr_req : std_logic;
  signal s_instr_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_instr_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_instr_ack : std_logic;

  -- Pipeline data bus master signals.
  signal s_data_req : std_logic;
  signal s_data_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_data_dat_w : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_data_we : std_logic;
  signal s_data_sel : std_logic_vector(C_WORD_SIZE/8-1 downto 0);
  signal s_data_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_data_ack : std_logic;
  signal s_data_busy : std_logic;
begin
  --------------------------------------------------------------------------------------------------
  -- Pipeline.
  --------------------------------------------------------------------------------------------------

  pipeline_1: entity work.pipeline
    generic map(
      CONFIG => CONFIG
    )
    port map (
      i_clk => i_clk,
      i_rst => i_rst,

      -- Cache control signals.
      o_invalidate_icache => s_invalidate_icache,
      o_invalidate_dcache => s_invalidate_dcache,
      o_flush_dcache => s_flush_dcache,

      -- Instruction interface.
      o_instr_req => s_instr_req,
      o_instr_adr => s_instr_adr,
      i_instr_dat => s_instr_dat,
      i_instr_ack => s_instr_ack,

      -- Data interface.
      o_data_req => s_data_req,
      o_data_adr => s_data_adr,
      o_data_dat => s_data_dat_w,
      o_data_we => s_data_we,
      o_data_sel => s_data_sel,
      i_data_dat => s_data_dat,
      i_data_ack => s_data_ack,
      i_data_busy => s_data_busy,

      -- Debug trace interface.
      o_debug_trace => o_debug_trace
    );


  --------------------------------------------------------------------------------------------------
  -- Caches.
  --------------------------------------------------------------------------------------------------

  -- Instruction L1 cache.
  l1i: entity work.icache
    generic map(
      CONFIG => CONFIG
    )
    port map (
      i_clk => i_clk,
      i_rst => i_rst,
      i_invalidate => s_invalidate_icache,

      -- From instruction fetch.
      i_instr_req => s_instr_req,
      i_instr_adr => s_instr_adr,
      o_instr_dat => s_instr_dat,
      o_instr_ack => s_instr_ack,

      -- To external memory.
      o_mem_cyc => o_imem_cyc,
      o_mem_stb => o_imem_stb,
      o_mem_adr => o_imem_adr,
      i_mem_dat => i_imem_dat,
      i_mem_ack => i_imem_ack,
      i_mem_stall => i_imem_stall,
      i_mem_err => i_imem_err
    );

  -- Data L1 cache.
  l1d: entity work.dcache
    generic map(
      CONFIG => CONFIG
    )
    port map (
      i_clk => i_clk,
      i_rst => i_rst,
      i_invalidate => s_invalidate_dcache,
      i_flush => s_flush_dcache,

      -- From data.
      i_data_req => s_data_req,
      i_data_adr => s_data_adr,
      i_data_dat => s_data_dat_w,
      i_data_we => s_data_we,
      i_data_sel => s_data_sel,
      o_data_dat => s_data_dat,
      o_data_ack => s_data_ack,
      o_data_busy => s_data_busy,

      -- To external memory.
      o_mem_cyc => o_dmem_cyc,
      o_mem_stb => o_dmem_stb,
      o_mem_adr => o_dmem_adr,
      o_mem_dat => o_dmem_dat,
      o_mem_we => o_dmem_we,
      o_mem_sel => o_dmem_sel,
      i_mem_dat => i_dmem_dat,
      i_mem_ack => i_dmem_ack,
      i_mem_stall => i_dmem_stall,
      i_mem_err => i_dmem_err
    );

end rtl;
