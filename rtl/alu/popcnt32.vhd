----------------------------------------------------------------------------------------------------
-- Copyright (c) 2021 Marcus Geelnard
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

entity popcnt32 is
  generic(
    CONFIG : T_CORE_CONFIG
  );
  port(
    i_src : in std_logic_vector(31 downto 0);
    i_packed_mode : in T_PACKED_MODE;
    o_result : out std_logic_vector(31 downto 0)
  );
end popcnt32;


---------------------------------------------------------------------------------------------------
-- The popcnt (population count) function is implemented as a 5-level adder tree.
---------------------------------------------------------------------------------------------------

architecture rtl of popcnt32 is
  -- Level 1
  subtype t_uint2 is unsigned(1 downto 0);
  type t_lvl1_array is array (15 downto 0) of t_uint2;
  signal s_c1 : t_lvl1_array;

  -- Level 2
  subtype t_uint3 is unsigned(2 downto 0);
  type t_lvl2_array is array (7 downto 0) of t_uint3;
  signal s_c2 : t_lvl2_array;

  -- Level 3
  subtype t_uint4 is unsigned(3 downto 0);
  type t_lvl3_array is array (3 downto 0) of t_uint4;
  signal s_c3 : t_lvl3_array;

  -- Level 4
  subtype t_uint5 is unsigned(4 downto 0);
  type t_lvl4_array is array (1 downto 0) of t_uint5;
  signal s_c4 : t_lvl4_array;

  -- Level 5
  subtype t_uint6 is unsigned(5 downto 0);
  signal s_c5 : t_uint6;

  signal s_result_32 : std_logic_vector(31 downto 0);
  signal s_result_16 : std_logic_vector(31 downto 0);
  signal s_result_8 : std_logic_vector(31 downto 0);

  function add2bits(a : std_logic; b : std_logic) return unsigned is
    variable av : std_logic_vector(1 downto 0);
    variable bv : std_logic_vector(1 downto 0);
  begin
    av := '0' & a;
    bv := '0' & b;
    return unsigned(av) + unsigned(bv);
  end function;
begin
  -- Level 1
  Lvl1Gen: for k in 15 downto 0 generate
    s_c1(k) <= add2bits(i_src(2*k+1), i_src(2*k));
  end generate;

  -- Level 2
  Lvl2Gen: for k in 7 downto 0 generate
    s_c2(k) <= ("0" & s_c1(2*k+1)) + ("0" & s_c1(2*k));
  end generate;

  -- Level 3
  Lvl3Gen: for k in 3 downto 0 generate
    s_c3(k) <= ("0" & s_c2(2*k+1)) + ("0" & s_c2(2*k));
  end generate;

  -- Level 4
  Lvl4Gen: for k in 1 downto 0 generate
    s_c4(k) <= ("0" & s_c3(2*k+1)) + ("0" & s_c3(2*k));
  end generate;

  -- Level 5 (final level)
  s_c5 <= ("0" & s_c4(1)) + ("0" & s_c4(0));

  -- 32-bit result.
  s_result_32(31 downto 6) <= (others => '0');
  s_result_32(5 downto 0) <= std_logic_vector(s_c5);

  PACKED_GEN: if CONFIG.HAS_PO generate
    -- 16x2-bit result.
    s_result_16(31 downto 21) <= (others => '0');
    s_result_16(20 downto 16) <= std_logic_vector(s_c4(1));
    s_result_16(15 downto 5) <= (others => '0');
    s_result_16(4 downto 0) <= std_logic_vector(s_c4(0));

    -- 8x4-bit result.
    s_result_8(31 downto 28) <= (others => '0');
    s_result_8(27 downto 24) <= std_logic_vector(s_c3(3));
    s_result_8(23 downto 20) <= (others => '0');
    s_result_8(19 downto 16) <= std_logic_vector(s_c3(2));
    s_result_8(15 downto 12) <= (others => '0');
    s_result_8(11 downto 8) <= std_logic_vector(s_c3(1));
    s_result_8(7 downto 4) <= (others => '0');
    s_result_8(3 downto 0) <= std_logic_vector(s_c3(0));

    -- Select outputs.
    o_result <= s_result_8  when i_packed_mode = C_PACKED_BYTE else
                s_result_16 when i_packed_mode = C_PACKED_HALF_WORD else
                s_result_32;
  else generate
    -- In unpacked mode we only have to consider the 32-bit result.
    o_result <= s_result_32;
  end generate;
end rtl;
