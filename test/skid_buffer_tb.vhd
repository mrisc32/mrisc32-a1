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

entity skid_buffer_tb is
end skid_buffer_tb;

architecture behavioral of skid_buffer_tb is
  signal s_clk : std_logic;
  signal s_rst : std_logic;
  signal s_stall : std_logic;
  signal s_d : std_logic_vector(3 downto 0);
  signal s_valid_w : std_logic;
  signal s_q : std_logic_vector(3 downto 0);
  signal s_valid : std_logic;
begin
  skid_buffer_0: entity work.skid_buffer
    generic map (
      WIDTH => 4
    )
    port map (
      i_clk => s_clk,
      i_rst => s_rst,
      i_stall => s_stall,
      i_d => s_d,
      i_valid => s_valid_w,
      o_q => s_q,
      o_valid => s_valid
    );

  process
    -- Patterns to apply.
    type pattern_type is record
      -- Inputs
      stall : std_logic;
      d : std_logic_vector(3 downto 0);
      valid_w : std_logic;

      -- Expected outputs
      q : std_logic_vector(3 downto 0);
      valid : std_logic;
    end record;
    type pattern_array is array (natural range <>) of pattern_type;
    constant patterns : pattern_array := (
        ('0', "1111", '1', "1111", '1'),
        ('0', "0100", '0', "0000", '0'),
        ('1', "0101", '0', "0000", '0'),
        ('1', "0110", '1', "0110", '1'),
        ('1', "0111", '0', "0110", '1'),
        ('1', "1000", '0', "0110", '1'),
        ('0', "1001", '0', "0110", '0'),
        ('0', "1010", '0', "0110", '0'),
        ('1', "1011", '0', "0110", '0'),
        ('1', "1100", '1', "1100", '1'),
        ('0', "1101", '1', "1101", '1'),
        ('0', "1110", '0', "1100", '0')
      );
  begin
    -- Start by resetting the entity (to have defined signals).
    s_rst <= '1';
    s_clk <= '0';
    s_stall <= '0';
    s_valid_w <= '0';
    s_d <= (others => '0');

    wait for 1 ns;
    s_rst <= '0';
    wait for 1 ns;

    -- Test all the patterns in the pattern array.
    for i in patterns'range loop
      -- Set the inputs.
      s_stall <= patterns(i).stall;
      s_valid_w <= patterns(i).valid_w;
      s_d <= patterns(i).d;

      -- Tick the clock.
      wait for 1 ns;
      s_clk <= '1';
      wait for 1 ns;
      s_clk <= '0';

      --  Check the outputs.
      assert s_q = patterns(i).q
        report "Bad q:" & lf &
               "  i = " & integer'image(i) & lf &
               "  q = " & to_string(s_q) & lf &
               " (expected " & to_string(patterns(i).q) & ")"
            severity error;
      assert s_valid = patterns(i).valid
        report "Bad valid:" & lf &
               "  i = " & integer'image(i) & lf &
               "  valid = " & to_string(s_valid) & lf &
               " (expected " & to_string(patterns(i).valid) & ")"
            severity error;
    end loop;
    assert false report "End of test" severity note;
    --  Wait forever; this will finish the simulation.
    wait;
  end process;
end behavioral;

