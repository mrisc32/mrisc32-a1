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
use work.config.all;
use work.types.all;

entity popcnt32_tb is
end popcnt32_tb;

architecture behavioral of popcnt32_tb is
  signal s_src : std_logic_vector(31 downto 0);
  signal s_packed_mode : T_PACKED_MODE;
  signal s_result : std_logic_vector(31 downto 0);
begin
  popcnt32_0: entity work.popcnt32
    generic map (
      CONFIG => C_CORE_CONFIG_FULL
    )
    port map (
      i_src => s_src,
      i_packed_mode => s_packed_mode,
      o_result => s_result
    );

  process
    type pattern_array is array (natural range <>) of std_logic_vector(31 downto 0);
    constant patterns : pattern_array := (
        ("00000000000000000000000000000000"),
        ("00000000000000000000000000000001"),
        ("00000000000000000000000000000010"),
        ("00001000100001000000010000100101"),
        ("10101010101010101010101010101010"),
        ("11100011111100011100110111001111"),
        ("11111111111111111111111111111111")
      );

    function refPopcnt32(x: std_logic_vector(31 downto 0)) return std_logic_vector is
      variable v_count : unsigned(31 downto 0);
    begin
      v_count := to_unsigned(0, 32);
      for i in 31 downto 0 loop
        if x(i) = '1' then
          v_count := v_count + 1;
        end if;
      end loop;
      return std_logic_vector(v_count);
    end function;

  begin
    -- Test some values from 0 to 2^32-1.
    for i in patterns'range loop
      for k in 0 to 10000 loop
        -- Set the input.
        s_src <= to_word(k*55) or patterns(i);
        s_packed_mode <= C_PACKED_NONE;  -- TODO(m): Add this to the test pattern.

        -- Wait for the results.
        wait for 1 ns;

        --  Check the output.
        assert s_result = refPopcnt32(s_src)
          report "Bad count value:" & lf &
                 "  src=" & to_string(s_src) & lf &
                 "  cnt=" & to_string(s_result) & " (expected " & to_string(refPopcnt32(s_src)) & ")"
            severity error;
      end loop;
    end loop;
    assert false report "End of test" severity note;
    --  Wait forever; this will finish the simulation.
    wait;
  end process;
end behavioral;

