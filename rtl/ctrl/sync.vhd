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
-- Synchronize instruction stream.
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.types.all;
use work.config.all;

entity sync is
  port(
    -- Control signals.
    i_clk : in std_logic;
    i_rst : in std_logic;

    -- Inputs.
    i_en : in std_logic;
    i_op : in T_SYNC_OP;

    -- Outputs.
    o_stall : out std_logic;
    o_done : out std_logic;
    o_cancel_pending_instructions : out std_logic;
    o_result : out std_logic_vector(C_WORD_SIZE-1 downto 0)
  );
end sync;

architecture rtl of sync is
  type T_STATE is (READY, STALLING, DONE);
  signal s_state : T_STATE;

  constant C_DELAY : integer := 1500;
  signal s_delay_cnt : integer range 0 to C_DELAY;

  signal s_sync_req : std_logic;
begin
  -- Did we get a SYNC request?
  -- Note: This signal is typically held high during the entire stall period.
  s_sync_req <= '1' when i_en = '1' and i_op = C_SYNC_SYNC else '0';

  -- Counter state machine: We just delay/stall for a number of cycles that will allow for memory
  -- writes to finish (ideally we should have some communication with the dcache logic & write
  -- queue). Once the delay is finished we send a request to cancel pending instructions in the
  -- pipeline.
  process(i_rst, i_clk)
  begin
    if i_rst = '1' then
      s_state <= READY;
      s_delay_cnt <= 0;
      o_done <= '0';
      o_cancel_pending_instructions <= '0';
    elsif rising_edge(i_clk) then
      -- Default values if not changed by the state machine.
      o_done <= '0';
      o_cancel_pending_instructions <= '0';

      case s_state is
        when READY =>
          s_delay_cnt <= 0;
          if s_sync_req = '1' then
            s_state <= STALLING;
          end if;

        when STALLING =>
          s_delay_cnt <= s_delay_cnt + 1;
          if s_delay_cnt = C_DELAY-1 then
            s_state <= DONE;
          end if;

        when DONE =>
          o_done <= '1';
          o_cancel_pending_instructions <= '1';
          s_state <= READY;
      end case;
    end if;
  end process;

  -- Shall we stall the pipeline?
  o_stall <= '1' when (s_state = READY and s_sync_req = '1') or s_state = STALLING else '0';

  -- The result is always zero.
  o_result <= (others => '0');
end rtl;
