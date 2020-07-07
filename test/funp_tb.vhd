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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.types.all;

--  A testbench has no ports.
entity funp_tb is
end funp_tb;

architecture behav of funp_tb is
  -- IEEE 754 binary-32
  constant WIDTH : positive := 32;
  constant EXP_BITS : positive := 8;
  constant EXP_BIAS : positive := 127;
  constant FRACT_BITS : positive := WIDTH - 1 - EXP_BITS;

  -- IEEE 754 binary-16
  constant PACKED_WIDTH : positive := 16;
  constant PACKED_EXP_BITS : positive := 5;
  constant PACKED_EXP_BIAS : positive := 15;
  constant PACKED_FRACT_BITS : positive := PACKED_WIDTH - 1 - PACKED_EXP_BITS;

  signal s_clk : std_logic;

  signal s_input : std_logic_vector(WIDTH-1 downto 0);
  signal s_extract_high : std_logic;
  signal s_result : std_logic_vector(WIDTH-1 downto 0);
begin
  --  Component instantiation.
  funp_0: entity work.funp
    port map (
      i_input => s_input,
      i_extract_high => s_extract_high,
      o_result => s_result
    );

  process
    --  The patterns to apply.
    type pattern_type is record
      -- Inputs.
      extract_high : std_logic;
      flt16_1 : std_logic_vector(PACKED_WIDTH-1 downto 0);
      flt16_2 : std_logic_vector(PACKED_WIDTH-1 downto 0);

      -- Expected outputs.
      result : std_logic_vector(WIDTH-1 downto 0);
    end record;
    type pattern_array is array (natural range <>) of pattern_type;
    constant patterns : pattern_array := (
        (
         -- LO(0.0, 0.0) = 0.0
         '0', 16X"0000", 16X"0000", 32X"00000000"
        ),
        (
         -- HI(0.0, 0.0) = 0.0
         '1', 16X"0000", 16X"0000", 32X"00000000"
        ),
        (
         -- LO(1.0, 2.0) = 2.0
         '0', 16X"3c00", 16X"4000", 32X"40000000"
        ),
        (
         -- HI(1.0, 2.0) = 1.0
         '1', 16X"3c00", 16X"4000", 32X"3f800000"
        ),
        (
         -- LO(-1.0, 3.998046875) = 3.998046875
         '0', 16X"bc00", 16X"43ff", 32X"407fe000"
        ),
        (
         -- HI(-1.0, 3.998046875) = -1.0
         '1', 16X"bc00", 16X"43ff", 32X"bf800000"
        ),
        (
         -- LO(-Inf, NaN) = NaN
         '0', 16X"fc00", 16X"7fff", 32X"7fffe000"
        ),
        (
         -- HI(-Inf, NaN) = -Inf
         '1', 16X"fc00", 16X"7fff", 32X"ff800000"
        )
      );
  begin
    -- Reset all inputs.
    s_extract_high <= '0';
    s_input <= (others => '0');

    -- Reset the entity.
    s_clk <= '0';
    wait for 1 ns;
    s_clk <= '1';
    wait until s_clk = '1';

    -- Test all the patterns in the pattern array.
    for i in patterns'range loop
      -- Prepare the input signal s.
      s_extract_high <= patterns(i).extract_high;
      s_input <= patterns(i).flt16_1 & patterns(i).flt16_2;

      -- Tick the clock (this is really unnecessary - just here for wave debugging).
      wait for 1 ns;
      s_clk <= '0';
      wait for 1 ns;
      s_clk <= '1';
      wait until s_clk = '1';

      --  Check the outputs.
      assert s_result = patterns(i).result
        report "Bad result (" & integer'image(i) & "):" & lf &
               "  r=" & to_string(s_result) &
               " (e=" & to_string(patterns(i).result) & ")"
            severity error;
    end loop;
    assert false report "End of test" severity note;
    --  Wait forever; this will finish the simulation.
    wait;
  end process;
end behav;
