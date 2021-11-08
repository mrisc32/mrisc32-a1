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
use work.types.all;
use work.config.all;

entity xchgsr is
  generic(
    CONFIG : T_CORE_CONFIG
  );
  port(
    i_reg_read : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_reg_write : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_we : in std_logic;
    o_result : out std_logic_vector(C_WORD_SIZE-1 downto 0)
  );
end xchgsr;

architecture rtl of xchgsr is
begin
  -- Read system register.
  process(i_reg_read)
  begin
    if i_reg_read = x"00000000" then
      -- CPU_FEATURES_0 (CPU feature flags register 0):
      --   0: VM (Vector operatoin module)
      --   1: PM (Packed operation module)
      --   2: FM (Floating-point module)
      --   3: SM (Saturating and halving arithmetic module)
      o_result(0) <= to_std_logic(CONFIG.HAS_VEC);
      o_result(1) <= to_std_logic(CONFIG.HAS_PO);
      o_result(2) <= to_std_logic(CONFIG.HAS_FP);
      o_result(3) <= to_std_logic(CONFIG.HAS_SA);
      o_result(C_WORD_SIZE-1 downto 4) <= (others => '0');
    elsif i_reg_read = x"00000010" then
      -- MAX_VL (Maximum vector length register).
      o_result <= to_word(C_VEC_REG_ELEMENTS);
    elsif i_reg_read = x"00000011" then
      -- LOG2_MAX_VL (Maximum vector length register).
      o_result <= to_word(C_LOG2_VEC_REG_ELEMENTS);
    else
      -- All reserved registers are zero.
      o_result <= (others => '0');
    end if;
  end process;

  -- TODO(m): Implement register write operations (we probably need clock and
  -- reset signals for that).
end rtl;
