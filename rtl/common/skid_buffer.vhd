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

----------------------------------------------------------------------------------------------------
-- This skid buffer implementation has the following behavior:
--
-- * When i_stall is asserted, the input signal is recorded if i_valid is asserted.
-- * The output signal is either the pass-through of the input signal, or a reproduction of the
--   last recorded input signal (during the first cycle that i_stall is de-asserted, unless i_valid
--   is asserted in which case the input signal is passed through).
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity skid_buffer is
  generic(
    WIDTH : integer := 1
  );
  port(
    i_clk : in std_logic;
    i_rst : in std_logic;
    i_stall : in std_logic;

    i_d : in std_logic_vector(WIDTH-1 downto 0);
    i_valid : in std_logic;

    o_q : out std_logic_vector(WIDTH-1 downto 0);
    o_valid : out std_logic
  );
end skid_buffer;

architecture rtl of skid_buffer is
  signal s_d_latched : std_logic_vector(WIDTH-1 downto 0);
  signal s_valid_latched : std_logic;
begin
  -- Latch D during stall.
  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_d_latched <= (others => '0');
      s_valid_latched <= '0';
    elsif rising_edge(i_clk) then
      if i_stall = '1' then
        if i_valid = '1' then
          s_d_latched <= i_d;
          s_valid_latched <= '1';
        end if;
      else
        s_valid_latched <= '0';
      end if;
    end if;
  end process;

  -- Select output signal.
  o_q <= i_d when i_valid = '1' else s_d_latched;
  o_valid <= i_valid or s_valid_latched;
end rtl;
