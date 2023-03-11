----------------------------------------------------------------------------------------------------
-- Copyright (c) 2023 Marcus Geelnard
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
-- Cache control: Implementation of the CCTRL instruction.
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.types.all;
use work.config.all;

entity cctrl is
  port(
    -- Control signals.
    i_clk : in std_logic;
    i_rst : in std_logic;

    -- Inputs.
    i_en : in std_logic;
    i_op : in T_CCTRL_OP;
    i_src_a : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_src_c : in std_logic_vector(C_WORD_SIZE-1 downto 0);

    -- Outputs.
    o_invalidate_icache : out std_logic;
    o_invalidate_dcache : out std_logic;
    o_invalidate_branch_predictor : out std_logic;
    o_flush_dcache : out std_logic;
    o_result : out std_logic_vector(C_WORD_SIZE-1 downto 0)
  );
end cctrl;

architecture rtl of cctrl is
begin
  process(i_rst, i_clk)
  begin
    if i_rst = '1' then
      o_invalidate_icache <= '0';
      o_invalidate_dcache <= '0';
      o_invalidate_branch_predictor <= '0';
      o_flush_dcache <= '0';
    elsif rising_edge(i_clk) then
      -- Default values when there is no CCTRL operation.
      o_invalidate_icache <= '0';
      o_invalidate_dcache <= '0';
      o_invalidate_branch_predictor <= '0';
      o_flush_dcache <= '0';

      if i_en = '1' and i_op = C_CCTRL_CCTRL then
        if i_src_a = 32x"000" then
          -- Invalidate entire icache.
          o_invalidate_icache <= '1';
        elsif i_src_a = 32x"001" then
          -- Invalidate entire dcache.
          o_invalidate_dcache <= '1';
        elsif i_src_a = 32x"002" then
          -- Invalidate entire branch predictor.
          o_invalidate_branch_predictor <= '1';
        elsif i_src_a = 32x"100" then
          -- Invalidate icache cache line given by address i_src_c.
          -- TODO(m): Implement me!
        elsif i_src_a = 32x"101" then
          -- Invalidate dcache cache line given by address i_src_c.
          -- TODO(m): Implement me!
        elsif i_src_a = 32x"201" then
          -- Flush entire dcache.
          o_flush_dcache <= '1';
        elsif i_src_a = 32x"301" then
          -- Flush dcache cache line given by address i_src_c.
          -- TODO(m): Implement me!
        end if;
      end if;
    end if;
  end process;

  -- The result of a CCTRL operation is always the input argument.
  o_result <= i_src_c;
end rtl;
