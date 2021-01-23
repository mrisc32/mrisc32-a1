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
    i_packed_mode : in T_PACKED_MODE;                        -- Packed mode
    o_result : out std_logic_vector(C_WORD_SIZE-1 downto 0)  -- ALU result
  );
end;

architecture rtl of alu is
  -- Intermediate (concurrent) operation results.
  signal s_cpuid_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_or_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_nor_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_and_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_bic_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_xor_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_set_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_min_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_max_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_minu_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_maxu_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_shuf_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_sel_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_rev_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_pack_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_ldli_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_ldhi_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_addhi_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_clz_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_popcnt_res : std_logic_vector(C_WORD_SIZE-1 downto 0);

  -- Signals for the packer.
  signal s_pack_is_saturated : std_logic;
  signal s_pack_is_unsigned : std_logic;

  -- Signals for the adder.
  signal s_add_res : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_sub_res : std_logic_vector(C_WORD_SIZE-1 downto 0);

  -- Signals for the shifter.
  signal s_shift_is_right : std_logic;
  signal s_shift_is_arithmetic : std_logic;
  signal s_shifter_res : std_logic_vector(C_WORD_SIZE-1 downto 0);

  -- Signals for 21-bit high immediate operations.
  signal s_src_b_as_imm21hi : std_logic_vector(C_WORD_SIZE-1 downto 0);

begin
  ------------------------------------------------------------------------------------------------
  -- CPUID
  ------------------------------------------------------------------------------------------------

  CPUID: entity work.cpuid
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_src_a => i_src_a,
      i_src_b => i_src_b,
      o_result => s_cpuid_res
    );


  ------------------------------------------------------------------------------------------------
  -- Bitwise operations
  ------------------------------------------------------------------------------------------------

  -- C_ALU_OR
  s_or_res <= i_src_a or i_src_b;

  -- C_ALU_NOR
  s_nor_res <= not (i_src_a or i_src_b);

  -- C_ALU_AND
  s_and_res <= i_src_a and i_src_b;

  -- C_ALU_BIC
  s_bic_res <= i_src_a and (not i_src_b);

  -- C_ALU_XOR
  s_xor_res <= i_src_a xor i_src_b;


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
  PackSaturatedMux: with i_op select
    s_pack_is_saturated <=
        '1' when C_ALU_PACKS | C_ALU_PACKSU,
        '0' when C_ALU_PACK,
        '-' when others;

  PackUnsignedMux: with i_op select
    s_pack_is_unsigned <=
        '1' when C_ALU_PACKSU,
        '0' when C_ALU_PACKS,
        '-' when others;

  AluPack: entity work.pack32
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_src_a => i_src_a,
      i_src_b => i_src_b,
      i_saturate => s_pack_is_saturated,
      i_unsigned => s_pack_is_unsigned,
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

  -- Note: These MUX:es should be optimized by the synthesis tool to only depend on a single
  -- bit of i_op.
  ShiftIsRightMux: with i_op select
    s_shift_is_right <=
        '1' when C_ALU_LSR | C_ALU_ASR,
        '0' when C_ALU_LSL,
        '-' when others;

  ShiftIsArithmeticMux: with i_op select
    s_shift_is_arithmetic <=
        '1' when C_ALU_ASR,
        '0' when C_ALU_LSL | C_ALU_LSR,
        '-' when others;

  AluShifter: entity work.shift32
    generic map (
      CONFIG => CONFIG
    )
    port map (
      i_right => s_shift_is_right,
      i_arithmetic => s_shift_is_arithmetic,
      i_src => i_src_a,
      i_shift => i_src_b,
      i_packed_mode => i_packed_mode,
      o_result => s_shifter_res
    );

  ------------------------------------------------------------------------------------------------
  -- 21-bit immediate operations
  ------------------------------------------------------------------------------------------------

  -- Expand 21-bit immediate according to LDHI/ADDPCHI semantics.
  s_src_b_as_imm21hi(C_WORD_SIZE-1 downto C_WORD_SIZE-21) <= i_src_b(20 downto 0);
  s_src_b_as_imm21hi(C_WORD_SIZE-22 downto 0) <= (others => i_src_b(0));

  -- C_ALU_LDLI, C_ALU_LDHI
  s_ldli_res <= i_src_b;
  s_ldhi_res <= s_src_b_as_imm21hi;

  -- Add high immediate (C_ALU_ADDHI)
  s_addhi_res <= std_logic_vector(unsigned(i_src_a) + unsigned(s_src_b_as_imm21hi));


  ------------------------------------------------------------------------------------------------
  -- Select the output.
  ------------------------------------------------------------------------------------------------

  AluMux: with i_op select
    o_result <=
        s_cpuid_res when C_ALU_CPUID,
        s_or_res  when C_ALU_OR,
        s_nor_res when C_ALU_NOR,
        s_and_res when C_ALU_AND,
        s_bic_res when C_ALU_BIC,
        s_xor_res when C_ALU_XOR,
        s_add_res when C_ALU_ADD,
        s_sub_res when C_ALU_SUB,
        s_set_res when C_ALU_SEQ | C_ALU_SNE | C_ALU_SLT | C_ALU_SLTU | C_ALU_SLE | C_ALU_SLEU,
        s_min_res when C_ALU_MIN,
        s_max_res when C_ALU_MAX,
        s_minu_res when C_ALU_MINU,
        s_maxu_res when C_ALU_MAXU,
        s_shifter_res when C_ALU_LSR | C_ALU_ASR | C_ALU_LSL,
        s_shuf_res when C_ALU_SHUF,
        s_sel_res when C_ALU_SEL,
        s_clz_res when C_ALU_CLZ,
        s_popcnt_res when C_ALU_POPCNT,
        s_rev_res when C_ALU_REV,
        s_pack_res when C_ALU_PACK | C_ALU_PACKS | C_ALU_PACKSU,
        s_ldli_res when C_ALU_LDLI,
        s_ldhi_res when C_ALU_LDHI,
        s_addhi_res when C_ALU_ADDHI,
        (others => '-') when others;

end rtl;

