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
-- This entity implements the PACK, PACKS, PACKSU, PACKHI, PACKHIR and PACKHIUR instructions.
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.types.all;
use work.config.all;

entity pack32 is
  generic(
    CONFIG : T_CORE_CONFIG
  );
  port(
    i_src_a       : in std_logic_vector(31 downto 0);
    i_src_b       : in std_logic_vector(31 downto 0);
    i_op          : in T_ALU_OP;
    i_packed_mode : in T_PACKED_MODE;
    o_result      : out std_logic_vector(31 downto 0)
  );
end pack32;

architecture rtl of pack32 is
  subtype T_PACK_OP is std_logic_vector(2 downto 0);
  constant C_PACK     : T_PACK_OP := C_ALU_PACK(2 downto 0);
  constant C_PACKS    : T_PACK_OP := C_ALU_PACKS(2 downto 0);
  constant C_PACKSU   : T_PACK_OP := C_ALU_PACKSU(2 downto 0);
  constant C_PACKHI   : T_PACK_OP := C_ALU_PACKHI(2 downto 0);
  constant C_PACKHIR  : T_PACK_OP := C_ALU_PACKHIR(2 downto 0);
  constant C_PACKHIUR : T_PACK_OP := C_ALU_PACKHIUR(2 downto 0);

  function extract_lo(x : std_logic_vector) return std_logic_vector is
  begin
    return x((x'left + x'right)/2 downto x'right);
  end function;

  function saturate(x : std_logic_vector) return std_logic_vector is
    constant C_SIZE : integer := x'length / 2;
    variable v_hi : std_logic_vector(C_SIZE downto 0);
  begin
    v_hi := x(x'left downto ((x'left+1 + x'right)/2)-1);
    if signed(v_hi) = 0 or signed(v_hi) = -1 then
      return extract_lo(x);
    elsif v_hi(v_hi'left) = '0' then
      -- Positive value -> Overflow.
      return '0' & std_logic_vector(to_signed(-1, C_SIZE-1));
    else
      -- Negative value -> Underflow.
      return '1' & std_logic_vector(to_signed(0, C_SIZE-1));
    end if;
  end function;

  function saturate_unsigned(x : std_logic_vector) return std_logic_vector is
    constant C_SIZE : integer := x'length / 2;
    variable v_hi : std_logic_vector(C_SIZE-1 downto 0);
  begin
    v_hi := x(x'left downto (x'left+1 + x'right)/2);
    if unsigned(v_hi) = 0 then
      return extract_lo(x);
    else
      return std_logic_vector(to_signed(-1, C_SIZE));
    end if;
  end function;

  function extract_hi(x : std_logic_vector) return std_logic_vector is
  begin
    return x(x'left downto (x'left+1 + x'right)/2);
  end function;

  function round_hi(x : std_logic_vector) return std_logic_vector is
    constant C_SIZE : integer := x'length / 2 + 1;
    variable v_hi : signed(C_SIZE-1 downto 0);
    variable v_hi_plus1 : signed(C_SIZE-1 downto 0);
  begin
    v_hi := signed(x(x'left downto (x'left+1 + x'right)/2-1));
    v_hi_plus1 := v_hi + 1;
    if v_hi(C_SIZE-1) = '0' and v_hi_plus1(C_SIZE-1) = '1' then
      return std_logic_vector(v_hi(C_SIZE-1 downto 1));  -- Overflow
    else
      return std_logic_vector(v_hi_plus1(C_SIZE-1 downto 1));
    end if;
  end function;

  function round_hi_unsigned(x : std_logic_vector) return std_logic_vector is
    constant C_SIZE : integer := x'length / 2 + 1;
    variable v_hi : unsigned(C_SIZE-1 downto 0);
    variable v_hi_plus1 : unsigned(C_SIZE-1 downto 0);
  begin
    v_hi := unsigned(x(x'left downto (x'left+1 + x'right)/2-1));
    v_hi_plus1 := v_hi + 1;
    if v_hi(C_SIZE-1) = '1' and v_hi_plus1(C_SIZE-1) = '0' then
      return std_logic_vector(v_hi(C_SIZE-1 downto 1));  -- Overflow
    else
      return std_logic_vector(v_hi_plus1(C_SIZE-1 downto 1));
    end if;
  end function;

  function pack(a : std_logic_vector; b : std_logic_vector;
                op : T_PACK_OP) return std_logic_vector is
  begin
    if op = C_PACKS then
      return saturate(a) & saturate(b);
    elsif op = C_PACKSU then
      return saturate_unsigned(a) & saturate_unsigned(b);
    elsif op = C_PACKHI then
      return extract_hi(a) & extract_hi(b);
    elsif op = C_PACKHIR then
      return round_hi(a) & round_hi(b);
    elsif op = C_PACKHIUR then
      return round_hi_unsigned(a) & round_hi_unsigned(b);
    else
      -- C_PACK
      return extract_lo(a) & extract_lo(b);
    end if;
  end function;
begin
  PACKED_GEN: if CONFIG.HAS_PO generate
    signal s_op : T_PACK_OP;
    signal s_res_32 : std_logic_vector(31 downto 0);
    signal s_res_16 : std_logic_vector(31 downto 0);
    signal s_res_8 : std_logic_vector(31 downto 0);
  begin
    SAT_GEN: if CONFIG.HAS_SA generate
      -- Select operation (map to C_PACK, C_PACKS or C_PACKSU).
      s_op <= i_op(2 downto 0);
    else generate
      -- Without support for saturating operations, we only implement PACK and PACKHI.
      s_op <= C_PACK when i_op = C_ALU_PACK else
              C_PACKHI when i_op = C_ALU_PACKHI else
              (others => '-');
    end generate;

    -- 32-bit pack.
    s_res_32 <= pack(i_src_a, i_src_b, s_op);

    -- 2x 16-bit pack.
    s_res_16(15 downto 0) <= pack(i_src_a(15 downto 0), i_src_b(15 downto 0), s_op);
    s_res_16(31 downto 16) <= pack(i_src_a(31 downto 16), i_src_b(31 downto 16), s_op);

    -- 4x 8-bit pack.
    s_res_8(7 downto 0) <= pack(i_src_a(7 downto 0), i_src_b(7 downto 0), s_op);
    s_res_8(15 downto 8) <= pack(i_src_a(15 downto 8), i_src_b(15 downto 8), s_op);
    s_res_8(23 downto 16) <= pack(i_src_a(23 downto 16), i_src_b(23 downto 16), s_op);
    s_res_8(31 downto 24) <= pack(i_src_a(31 downto 24), i_src_b(31 downto 24), s_op);

    -- Select the result based on the packed mode.
    PackModeMux: with i_packed_mode select
      o_result <=
          s_res_8 when C_PACKED_BYTE,
          s_res_16 when C_PACKED_HALF_WORD,
          s_res_32 when others;
  else generate
    o_result <= (others => '0');
  end generate;
end rtl;
