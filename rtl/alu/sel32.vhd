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
use work.types.all;

entity sel32 is
  port(
      i_src_a : in std_logic_vector(31 downto 0);
      i_src_b : in std_logic_vector(31 downto 0);
      i_src_c : in std_logic_vector(31 downto 0);
      i_packed_mode : in T_PACKED_MODE;
      o_result : out std_logic_vector(31 downto 0)
    );
end sel32;

architecture rtl of sel32 is
  signal s_sel_0 : std_logic_vector(31 downto 0);
  signal s_sel_1 : std_logic_vector(31 downto 0);
  signal s_sel_2 : std_logic_vector(31 downto 0);
  signal s_sel_3 : std_logic_vector(31 downto 0);
begin
  -- There are four different flavors of the SEL instruction, each with different
  -- permutations of the operands.
  s_sel_0 <= (i_src_a and i_src_c) or (i_src_b and not i_src_c);
  s_sel_1 <= (i_src_a and not i_src_c) or (i_src_b and i_src_c);
  s_sel_2 <= (i_src_c and i_src_a) or (i_src_b and not i_src_a);
  s_sel_3 <= (i_src_c and not i_src_a) or (i_src_b and i_src_a);

  SelMux: with i_packed_mode select
    o_result <=
        s_sel_0 when "00",
        s_sel_1 when "01",
        s_sel_2 when "10",
        s_sel_3 when "11",
        (others => '-') when others;
end rtl;
