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

----------------------------------------------------------------------------------------------------
-- Pipeline stages 1 & 2: IF1 (Program Counter) and IF2 (Instruction Fetch)
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.config.all;
use work.types.all;

entity fetch is
  generic(
    CONFIG : T_CORE_CONFIG
  );
  port(
    -- Control signals.
    i_clk : in std_logic;
    i_rst : in std_logic;
    i_stall : in std_logic;
    i_cancel : in std_logic;

    -- Results from the branch/PC correction unit in the EX stage (async).
    i_pccorr_source : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_pccorr_target : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_pccorr_branch_type : in T_BRANCH_TYPE;
    i_pccorr_is_taken : in std_logic;
    i_pccorr_adjust : in std_logic;  -- 1 if the PC correction needs to be applied.
    i_pccorr_adjusted_pc : in std_logic_vector(C_WORD_SIZE-1 downto 0);

    -- Instruction cache interface.
    o_cache_req : out std_logic;
    o_cache_adr : out std_logic_vector(C_WORD_SIZE-1 downto 2);
    i_cache_dat : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_cache_ack : in std_logic;

    -- To ID stage (sync).
    o_pc : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_instr : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_bubble : out std_logic  -- 1 if IF could not provide a new instruction.
  );
end fetch;

architecture rtl of fetch is
  constant C_NOP_INSTR : std_logic_vector(C_WORD_SIZE-1 downto 0) := x"00000011";

  type T_IF_STATE is (
    RESET1,
    RESET2,
    FETCH_INSTR
  );

  signal s_next_state : T_IF_STATE;
  signal s_state : T_IF_STATE;

  signal s_bp_read_en : std_logic;
  signal s_bp_read_pc : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_bp_taken : std_logic;
  signal s_bp_target : std_logic_vector(C_WORD_SIZE-1 downto 0);

  signal s_read_pc : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_read_en : std_logic;
  signal s_prev_read_pc : std_logic_vector(C_WORD_SIZE-1 downto 0);

  signal s_latched_instr : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_latched_pc : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_has_latched_instr : std_logic;

  signal s_instr : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_pc : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_bubble : std_logic;
begin
  --------------------------------------------------------------------------------------------------
  -- Branch predictor (optional).
  --------------------------------------------------------------------------------------------------

  BP_GEN: if CONFIG.ENABLE_BRANCH_PREDICTOR generate
    -- Instantiate the branch target buffer.
    s_bp_read_en <= s_read_en;
    s_bp_read_pc <= s_read_pc;
    predictor_0: entity work.branch_predictor
      port map (
        -- Control signals.
        i_clk => i_clk,
        i_rst => i_rst,
        i_invalidate => '0',
        i_cancel_speculation => i_cancel,

        -- Buffer lookup (sync).
        i_read_pc => s_bp_read_pc,
        i_read_en => s_bp_read_en,
        o_predict_taken => s_bp_taken,
        o_predict_target => s_bp_target,

        -- Buffer update (sync).
        i_write_pc => i_pccorr_source,
        i_write_branch_type => i_pccorr_branch_type,
        i_write_is_taken => i_pccorr_is_taken,
        i_write_target => i_pccorr_target
      );
  else generate
    -- Predict nothing.
    s_bp_taken <= '0';
    s_bp_target <= (others => '0');
  end generate;


  --------------------------------------------------------------------------------------------------
  -- State machine.
  --------------------------------------------------------------------------------------------------

  process(ALL)
  begin
    case s_state is
      when RESET1 =>
        s_read_pc <= CONFIG.RESET_PC;
        s_read_en <= '0';
        s_instr <= C_NOP_INSTR;
        s_pc <= (others => '0');
        s_bubble <= '1';
        s_next_state <= RESET2;

      when RESET2 =>
        s_read_pc <= CONFIG.RESET_PC;
        s_read_en <= '1';
        s_instr <= C_NOP_INSTR;
        s_pc <= (others => '0');
        s_bubble <= '1';
        s_next_state <= FETCH_INSTR;

      when FETCH_INSTR =>
        -- Did we get an instruction from the cache?
        if i_cache_ack = '1' and i_cancel = '0' then
          s_instr <= i_cache_dat;
          s_pc <= s_prev_read_pc;
          s_bubble <= '0';
        elsif s_has_latched_instr = '1' and i_cancel = '0' then
          s_instr <= s_latched_instr;
          s_pc <= s_latched_pc;
          s_bubble <= '0';
        else
          s_instr <= C_NOP_INSTR;
          s_pc <= s_prev_read_pc;
          s_bubble <= '1';
        end if;

        -- Can we make a new cache request?
        -- Note: We start a request even under stall, given the right circumstances.
        if i_stall = '0' or (i_cache_ack = '0' and s_has_latched_instr = '0') or (i_pccorr_adjust = '1') then
          s_read_en <= '1';
        else
          s_read_en <= '0';
        end if;

        -- Update the PC.
        if i_pccorr_adjust = '1' then
          -- Correct the PC (we had a branch misprediction).
          s_read_pc <= i_pccorr_adjusted_pc;
        elsif i_cache_ack = '1' then
          -- We got a new instruction from the cache.
          if s_bp_taken = '1' then
            s_read_pc <= s_bp_target;
          else
            s_read_pc <= std_logic_vector(unsigned(s_prev_read_pc) + 4);
          end if;
        else
          -- Repeat last request if we got no ack from the cache.
          s_read_pc <= s_prev_read_pc;
        end if;

        s_next_state <= FETCH_INSTR;

      when others =>
        s_read_pc <= (others => '0');
        s_read_en <= '0';
        s_instr <= C_NOP_INSTR;
        s_pc <= (others => '0');
        s_bubble <= '1';
        s_next_state <= RESET2;
    end case;
  end process;

  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_prev_read_pc <= (others => '0');
      s_latched_instr <= C_NOP_INSTR;
      s_latched_pc <= (others => '0');
      s_has_latched_instr <= '0';
      o_instr <= C_NOP_INSTR;
      o_pc <= (others => '0');
      o_bubble <= '1';
      s_state <= RESET1;
    elsif rising_edge(i_clk) then
      -- Pipeline stage outputs (clock synchronous).
      if i_stall = '0' then
        o_instr <= s_instr;
        o_pc <= s_pc;
        o_bubble <= s_bubble;
      end if;

      -- State update.
      s_prev_read_pc <= s_read_pc;
      s_state <= s_next_state;

      -- Latch results from the cache if we're stalled.
      if i_cancel = '1' then
        s_has_latched_instr <= '0';
      elsif i_stall = '1' then
        if i_cache_ack = '1' then
          s_latched_instr <= i_cache_dat;
          s_latched_pc <= s_prev_read_pc;
          s_has_latched_instr <= '1';
        end if;
      else
        s_has_latched_instr <= '0';
      end if;
    end if;
  end process;

  -- Cache interface outputs.
  o_cache_req <= s_read_en;
  o_cache_adr <= s_read_pc(C_WORD_SIZE-1 downto 2);
end rtl;
