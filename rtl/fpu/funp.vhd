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
-- This entity implements FUNPL and FUNPH (unpack floating-point from low and high half,
-- respectively).
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.types.all;

entity funp is
  generic(
    WIDTH : positive := 32;
    EXP_BITS : positive := 8;
    EXP_BIAS : positive := 127;
    FRACT_BITS : positive := 23;

    PACKED_WIDTH : positive := 16;
    PACKED_EXP_BITS : positive := 5;
    PACKED_EXP_BIAS : positive := 15;
    PACKED_FRACT_BITS : positive := 10
  );
  port(
    i_input : in std_logic_vector(WIDTH-1 downto 0);
    i_extract_high : in std_logic;
    o_result : out std_logic_vector(WIDTH-1 downto 0)
  );
end funp;

architecture rtl of funp is
  function unpack(x : std_logic_vector(PACKED_WIDTH-1 downto 0)) return std_logic_vector is
    variable v_sign : std_logic;
    variable v_packed_exp : std_logic_vector(PACKED_EXP_BITS-1 downto 0);
    variable v_packed_fract : std_logic_vector(PACKED_FRACT_BITS-1 downto 0);

    variable v_exp_is_all_ones : boolean;
    variable v_exp_is_all_zero : boolean;

    variable v_exp : std_logic_vector(EXP_BITS-1 downto 0);
    variable v_fract : std_logic_vector(FRACT_BITS-1 downto 0);
  begin
    -- Decompose.
    v_sign := x(PACKED_WIDTH-1);
    v_packed_exp := x(PACKED_WIDTH-2 downto PACKED_FRACT_BITS);
    v_packed_fract := x(PACKED_FRACT_BITS-1 downto 0);

    -- Exponent.
    v_exp_is_all_ones := (v_packed_exp = (v_packed_exp'range => '1'));
    v_exp_is_all_zero := (v_packed_exp = (v_packed_exp'range => '0'));
    if v_exp_is_all_ones then
      v_exp := (v_exp'range => '1');
    elsif v_exp_is_all_zero then
      v_exp := (v_exp'range => '0');
    else
      v_exp := std_logic_vector(resize(unsigned(v_packed_exp), EXP_BITS) - PACKED_EXP_BIAS + EXP_BIAS);
    end if;

    -- Fraction.
    v_fract := v_packed_fract & ((FRACT_BITS-PACKED_FRACT_BITS)-1 downto 0 => '0');

    return v_sign & v_exp & v_fract;
  end function;

  signal s_float_hi : std_logic_vector(PACKED_WIDTH-1 downto 0);
  signal s_float_lo : std_logic_vector(PACKED_WIDTH-1 downto 0);
begin
  -- Extract the high and low parts of the input word.
  s_float_hi <= i_input(WIDTH-1 downto WIDTH/2);
  s_float_lo <= i_input(WIDTH/2-1 downto 0);

  -- Unpack it.
  o_result <= unpack(s_float_hi) when i_extract_high = '1' else
              unpack(s_float_lo);
end rtl;
