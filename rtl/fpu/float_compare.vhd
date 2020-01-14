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

----------------------------------------------------------------------------------------------------
-- Compare floating point numbers.
----------------------------------------------------------------------------------------------------

entity float_compare is
  generic(
    WIDTH : positive := 32
  );
  port(
    i_src_a : in std_logic_vector(WIDTH-1 downto 0);
    i_src_b : in std_logic_vector(WIDTH-1 downto 0);

    i_props_a : T_FLOAT_PROPS;
    i_props_b : T_FLOAT_PROPS;

    o_magn_lt : out std_logic;
    o_eq : out std_logic;
    o_ne : out std_logic;
    o_lt : out std_logic;
    o_le : out std_logic
  );
end float_compare;

architecture rtl of float_compare is
  signal s_magn_a : unsigned(WIDTH-2 downto 0);
  signal s_magn_b : unsigned(WIDTH-2 downto 0);
  signal s_both_zero : std_logic;
  signal s_any_nan : std_logic;
  signal s_magn_eq : std_logic;
  signal s_magn_lt : std_logic;

  signal s_eq : std_logic;
  signal s_lt : std_logic;
begin
  -- Extract the raw magnitudes.
  s_magn_a <= unsigned(i_src_a(WIDTH-2 downto 0));
  s_magn_b <= unsigned(i_src_b(WIDTH-2 downto 0));

  -- According to IEEE 754, -0 == +0, which is a special case.
  s_both_zero <= i_props_a.is_zero and i_props_b.is_zero;

  -- According to IEEE 754, we need special treatment of NaN:
  --  - NaN != x for any x (including NaN).
  --  - NaN is never less than or greater than any x (including NaN).
  s_any_nan <= i_props_a.is_nan or i_props_b.is_nan;

  -- Compare exponents and magnitudes.
  s_magn_eq <= '1' when s_magn_a = s_magn_b else '0';
  s_magn_lt <= '1' when s_magn_a < s_magn_b else '0';

  -- Equal?
  s_eq <= (s_magn_eq and (not (i_props_a.is_neg xor i_props_b.is_neg))) or
          s_both_zero;

  -- Less than?
  s_lt <= '0' when s_both_zero = '1' else
          s_magn_lt when (i_props_a.is_neg = '0' and i_props_b.is_neg = '0') else
          (not (s_magn_lt or s_eq)) when (i_props_a.is_neg = '1' and i_props_b.is_neg = '1') else
          '1' when (i_props_a.is_neg = '1' and i_props_b.is_neg = '0') else
          '0';

  -- Outputs.
  o_magn_lt <= s_magn_lt;
  o_eq <= s_eq and not s_any_nan;
  o_ne <= (not s_eq) or s_any_nan;
  o_lt <= s_lt and not s_any_nan;
  o_le <= (s_eq or s_lt) and not s_any_nan;
end rtl;
