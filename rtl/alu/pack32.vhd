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
-- This entity implements the PACK, PACKS and PACKSU instructions.
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
    i_saturate    : in std_logic;
    i_unsigned    : in std_logic;
    i_packed_mode : in T_PACKED_MODE;
    o_result      : out std_logic_vector(31 downto 0)
  );
end pack32;

architecture rtl of pack32 is
  constant C_PACK : std_logic_vector(1 downto 0) := "00";
  constant C_PACKS : std_logic_vector(1 downto 0) := "10";
  constant C_PACKSU : std_logic_vector(1 downto 0) := "11";

  signal s_op : std_logic_vector(1 downto 0);
  signal s_res_32 : std_logic_vector(31 downto 0);

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

  function pack(a : std_logic_vector; b : std_logic_vector;
                op : std_logic_vector) return std_logic_vector is
  begin
    if op = C_PACKS then
      return saturate(a) & saturate(b);
    elsif op = C_PACKSU then
      return saturate_unsigned(a) & saturate_unsigned(b);
    else
      return extract_lo(a) & extract_lo(b);
    end if;
  end function;
begin
  SAT_GEN: if CONFIG.HAS_SA generate
    -- Select operation (map to C_PACK, C_PACKS or C_PACKSU).
    s_op <= i_saturate & i_unsigned;
  else generate
    -- Without support for saturating operations, we only implement PACK.
    s_op <= C_PACK;
  end generate;

  -- 32-bit pack.
  s_res_32 <= pack(i_src_a, i_src_b, s_op);

  PACKED_GEN: if CONFIG.HAS_PO generate
    signal s_res_16 : std_logic_vector(31 downto 0);
    signal s_res_8 : std_logic_vector(31 downto 0);
  begin
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
    -- In unpacked mode we only have to consider the 32-bit result.
    o_result <= s_res_32;
  end generate;
end rtl;
