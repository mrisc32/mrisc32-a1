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
use work.types.all;
use work.config.all;

entity alu is
  generic(
    CONFIG : T_CORE_CONFIG
  );
  port(
    i_op : in T_ALU_OP;                                      -- Operation
    i_src_a : in std_logic_vector(C_WORD_SIZE-1 downto 0);   -- Source operand A
    i_src_b : in std_logic_vector(C_WORD_SIZE-1 downto 0);   -- Source operand B
    i_src_c : in std_logic_vector(C_WORD_SIZE-1 downto 0);   -- Source operand C
    i_src_a_is_z : in std_logic;                             -- Is A reg Z?
    i_packed_mode : in T_PACKED_MODE;                        -- Packed mode
    o_result : out std_logic_vector(C_WORD_SIZE-1 downto 0)  -- ALU result
  );
end;

architecture rtl of alu is
  -- Intermediate (concurrent) operation results.
  signal s_xchgsr_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_and_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_or_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_xor_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_shifter_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_add_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_sub_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_min_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_max_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_minu_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_maxu_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_sel_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_shuf_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_set_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_pack_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_rev_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_clz_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_popcnt_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_ldi_res : std_logic_vector(C_WORD_SIZE-1 downto 0);

  -- Signals for XCHGSR.
  signal s_xchgsr_we : std_logic;

  -- Signals for the bitwise operations.
  signal s_bitwise_a : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_bitwise_b : std_logic_vector(C_WORD_SIZE-1 downto 0);

begin
  ------------------------------------------------------------------------------------------------
  -- XCHGSR
  ------------------------------------------------------------------------------------------------

  s_xchgsr_we <= not i_src_a_is_z;

  XCHGSR: entity work.xchgsr
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_reg_read => i_src_b,
      i_reg_write => i_src_a,
      i_we => s_xchgsr_we,
      o_result => s_xchgsr_res
    );


  ------------------------------------------------------------------------------------------------
  -- Bitwise operations
  ------------------------------------------------------------------------------------------------

  -- Optionally negate input signals for the bitwise operations.
  s_bitwise_a <= i_src_a when i_packed_mode(1) = '0' else not i_src_a;
  s_bitwise_b <= i_src_b when i_packed_mode(0) = '0' else not i_src_b;

  -- C_ALU_OR
  s_or_res <= s_bitwise_a or s_bitwise_b;

  -- C_ALU_AND
  s_and_res <= s_bitwise_a and s_bitwise_b;

  -- C_ALU_XOR
  s_xor_res <= s_bitwise_a xor s_bitwise_b;


  ------------------------------------------------------------------------------------------------
  -- Bit, byte and word shuffling
  ------------------------------------------------------------------------------------------------

  -- C_ALU_SHUF
  AluSHUF32: entity work.shuf32
    port map (
      i_src_a => i_src_a,
      i_src_b => i_src_b,
      o_result => s_shuf_res
    );

  -- C_ALU_SEL
  AluSEL32: entity work.sel32
    port map (
      i_src_a => i_src_a,
      i_src_b => i_src_b,
      i_src_c => i_src_c,
      i_packed_mode => i_packed_mode,
      o_result => s_sel_res
    );

  -- C_ALU_REV
  Rev: entity work.rev32
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_src => i_src_a,
      i_packed_mode => i_packed_mode,
      o_result => s_rev_res
    );

  -- C_ALU_PACK
  AluPack: entity work.pack32
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_src_a => i_src_a,
      i_src_b => i_src_b,
      i_op => i_op,
      i_packed_mode => i_packed_mode,
      o_result => s_pack_res
    );

  -- C_ALU_CLZ
  AluCLZ32: entity work.clz32
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_src => i_src_a,
      i_packed_mode => i_packed_mode,
      o_result => s_clz_res
    );

  -- C_ALU_POPCNT
  AluPOPCNT32: entity work.popcnt32
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_src => i_src_a,
      i_packed_mode => i_packed_mode,
      o_result => s_popcnt_res
    );

  ------------------------------------------------------------------------------------------------
  -- Arithmetic operations
  ------------------------------------------------------------------------------------------------

  -- Add/sub.
  Adder: entity work.add32
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_src_a => i_src_a,
      i_src_b => i_src_b,
      i_packed_mode => i_packed_mode,
      o_result => s_add_res
    );

  Subber: entity work.sub32
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_src_a => i_src_a,
      i_src_b => i_src_b,
      i_packed_mode => i_packed_mode,
      o_result => s_sub_res
    );

  -- Comparison operations.
  Compare: entity work.cmp32
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_src_a => i_src_a,
      i_src_b => i_src_b,
      i_op => i_op,
      i_packed_mode => i_packed_mode,
      o_set_res => s_set_res,
      o_min_res => s_min_res,
      o_max_res => s_max_res,
      o_minu_res => s_minu_res,
      o_maxu_res => s_maxu_res
    );

  ------------------------------------------------------------------------------------------------
  -- Shift operations
  ------------------------------------------------------------------------------------------------

  AluShifter: entity work.shift32
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_op => i_op,
      i_src => i_src_a,
      i_ctrl => i_src_b,
      i_dst => i_src_c,
      i_packed_mode => i_packed_mode,
      o_result => s_shifter_res
    );

  ------------------------------------------------------------------------------------------------
  -- Immediate operations
  ------------------------------------------------------------------------------------------------

  -- C_ALU_LDI (this is just a "move" instruction, i.e. load the immediate value)
  s_ldi_res <= i_src_b;

  ------------------------------------------------------------------------------------------------
  -- Select the output.
  ------------------------------------------------------------------------------------------------

  AluMux: with i_op select
    o_result <=
        s_xchgsr_res when C_ALU_XCHGSR,
        s_and_res when C_ALU_AND,
        s_or_res  when C_ALU_OR,
        s_xor_res when C_ALU_XOR,
        s_shifter_res when C_ALU_EBF | C_ALU_EBFU | C_ALU_MKBF | C_ALU_IBF,
        s_add_res when C_ALU_ADD,
        s_sub_res when C_ALU_SUB,
        s_min_res when C_ALU_MIN,
        s_max_res when C_ALU_MAX,
        s_minu_res when C_ALU_MINU,
        s_maxu_res when C_ALU_MAXU,
        s_sel_res when C_ALU_SEL,
        s_shuf_res when C_ALU_SHUF,
        s_set_res when C_ALU_SEQ | C_ALU_SNE | C_ALU_SLT | C_ALU_SLTU | C_ALU_SLE | C_ALU_SLEU,
        s_pack_res when C_ALU_PACK | C_ALU_PACKS | C_ALU_PACKSU |
                        C_ALU_PACKHI | C_ALU_PACKHIR | C_ALU_PACKHIUR,
        s_rev_res when C_ALU_REV,
        s_clz_res when C_ALU_CLZ,
        s_popcnt_res when C_ALU_POPCNT,
        s_ldi_res when C_ALU_LDI,
        (others => '-') when others;

end rtl;

