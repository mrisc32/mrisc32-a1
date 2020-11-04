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

----------------------------------------------------------------------------------------------------
-- This is a single CPU core, with a von Neumann architecture (with a single memory interface).
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.config.all;
use work.debug.all;

entity core_1mem is
  generic(
    CONFIG : T_CORE_CONFIG := C_CORE_CONFIG_DEFAULT
  );
  port(
    -- Control signals.
    i_clk : in std_logic;
    i_rst : in std_logic;

    -- Memory interface (Wishbone B4 pipelined master).
    o_mem_cyc : out std_logic;
    o_mem_stb : out std_logic;
    o_mem_adr : out std_logic_vector(C_WORD_SIZE-1 downto 2);
    o_mem_dat : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_mem_we : out std_logic;
    o_mem_sel : out std_logic_vector(C_WORD_SIZE/8-1 downto 0);
    i_mem_dat : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_mem_ack : in std_logic;
    i_mem_stall : in std_logic;
    i_mem_err : in std_logic;

    -- Debug trace interface.
    o_debug_trace : out T_DEBUG_TRACE
  );
end core_1mem;

architecture rtl of core_1mem is
  -- L1I master signals.
  signal s_imem_cyc : std_logic;
  signal s_imem_stb : std_logic;
  signal s_imem_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_imem_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_imem_ack : std_logic;
  signal s_imem_stall : std_logic;
  signal s_imem_err : std_logic;

  -- L1D master signals.
  signal s_dmem_cyc : std_logic;
  signal s_dmem_stb : std_logic;
  signal s_dmem_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_dmem_dat_w : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_dmem_we : std_logic;
  signal s_dmem_sel : std_logic_vector(C_WORD_SIZE/8-1 downto 0);
  signal s_dmem_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_dmem_ack : std_logic;
  signal s_dmem_stall : std_logic;
  signal s_dmem_err : std_logic;
begin
  --------------------------------------------------------------------------------------------------
  -- Pipeline.
  --------------------------------------------------------------------------------------------------

  core_1: entity work.core
    generic map(
      CONFIG => CONFIG
    )
    port map (
      i_clk => i_clk,
      i_rst => i_rst,

      -- Instruction interface.
      o_imem_cyc => s_imem_cyc,
      o_imem_stb => s_imem_stb,
      o_imem_adr => s_imem_adr,
      i_imem_dat => s_imem_dat,
      i_imem_ack => s_imem_ack,
      i_imem_stall => s_imem_stall,
      i_imem_err => s_imem_err,

      -- Data interface.
      o_dmem_cyc => s_dmem_cyc,
      o_dmem_stb => s_dmem_stb,
      o_dmem_adr => s_dmem_adr,
      o_dmem_dat => s_dmem_dat_w,
      o_dmem_we => s_dmem_we,
      o_dmem_sel => s_dmem_sel,
      i_dmem_dat => s_dmem_dat,
      i_dmem_ack => s_dmem_ack,
      i_dmem_stall => s_dmem_stall,
      i_dmem_err => s_dmem_err,

      -- Debug trace interface.
      o_debug_trace => o_debug_trace
    );

  -- Memory arbiter.
  arbiter_1: entity work.mem_arbiter
    port map (
      i_clk => i_clk,
      i_rst => i_rst,

      -- Instruction interface.
      i_instr_cyc => s_imem_cyc,
      i_instr_stb => s_imem_stb,
      i_instr_adr => s_imem_adr,
      o_instr_dat => s_imem_dat,
      o_instr_ack => s_imem_ack,
      o_instr_stall => s_imem_stall,
      o_instr_err => s_imem_err,

      -- Data interface.
      i_data_cyc => s_dmem_cyc,
      i_data_stb => s_dmem_stb,
      i_data_we => s_dmem_we,
      i_data_sel => s_dmem_sel,
      i_data_adr => s_dmem_adr,
      i_data_dat_w => s_dmem_dat_w,
      o_data_dat => s_dmem_dat,
      o_data_ack => s_dmem_ack,
      o_data_stall => s_dmem_stall,
      o_data_err => s_dmem_err,

      -- Memory interface.
      o_mem_cyc => o_mem_cyc,
      o_mem_stb => o_mem_stb,
      o_mem_we => o_mem_we,
      o_mem_sel => o_mem_sel,
      o_mem_adr => o_mem_adr,
      o_mem_dat_w => o_mem_dat,
      i_mem_dat => i_mem_dat,
      i_mem_ack => i_mem_ack,
      i_mem_stall => i_mem_stall,
      i_mem_err => i_mem_err
  );

end rtl;
