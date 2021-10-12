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
-- Branch Predictor
--
-- This is a configurable branch predictor with a branch target buffer. The PC provided by
-- i_read_pc represents the PC that will be used during the next cycle in the IF stage, and the
-- predicted target (and whether it should be taken) is provided during the next cycle.
--
-- The branch predictor is guaranteed to never signal o_predict_taken if a branch instruction has
-- not previously been seen at the given PC (if the program memory has been modified, i_invalidate
-- must be asserted to clear the predictor state).
--
-- Note: The two least significant bits of the PC are ignored (treated as zero).
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.config.all;
use work.types.all;

entity branch_predictor is
  port(
      -- Control signals.
      i_clk : in std_logic;
      i_rst : in std_logic;
      i_invalidate : in std_logic;
      i_cancel_speculation : in std_logic;

      -- Buffer lookup (sync).
      i_read_pc : in std_logic_vector(C_WORD_SIZE-1 downto 0);
      i_read_en : in std_logic;
      o_predict_taken : out std_logic;
      o_predict_target : out std_logic_vector(C_WORD_SIZE-1 downto 0);

      -- Buffer update (sync).
      i_write_pc : in std_logic_vector(C_WORD_SIZE-1 downto 0);
      i_write_branch_type : in T_BRANCH_TYPE;
      i_write_is_taken : in std_logic;
      i_write_target : in std_logic_vector(C_WORD_SIZE-1 downto 0)
    );
end branch_predictor;

architecture rtl of branch_predictor is
  -- Number of entries in the global history buffer (0 to disable).
  constant C_GHR_BITS : integer := 0;

  -- Total number of entries in the branch target buffer.
  constant C_LOG2_ENTRIES : integer := 9;  -- 512 entries.
  constant C_NUM_ENTRIES : integer := 2**C_LOG2_ENTRIES;

  -- Size of the tag.
  constant C_TAG_SIZE : integer := C_WORD_SIZE-2 - (C_LOG2_ENTRIES - C_GHR_BITS);

  -- Size of a branch target entry.
  -- Encoding of an entry: branch_type (2 bits) & is_taken (1 bit) & target_address
  constant C_ENTRY_SIZE : integer := 3 + C_WORD_SIZE-2;

  -- Number of bits per counter.
  constant C_COUNTER_SIZE : integer := 2;
  constant C_MAX_COUNT : integer := (2**C_COUNTER_SIZE) - 1;

  -- Number of entries in the return address stack (C_LOG2_RAS_ENTRIES = 0 to disable).
  -- Note: 8-16 entries is usually "good enough", but if the stack is synthesized as BRAM you can
  -- usually go much higher in order to utilize more of the RAM bits (that would otherwise be
  -- wasted).
  constant C_LOG2_RAS_ENTRIES : integer := 4;
  constant C_NUM_RAS_ENTRIES : integer := 2**C_LOG2_RAS_ENTRIES;

  type T_RAS_ARRAY is array (0 to C_NUM_RAS_ENTRIES-1) of std_logic_vector(C_WORD_SIZE-3 downto 0);
  signal s_ras_array : T_RAS_ARRAY;
  signal s_ras_index : unsigned(C_LOG2_RAS_ENTRIES-1 downto 0);
  signal s_ras_correct_index : unsigned(C_LOG2_RAS_ENTRIES-1 downto 0);
  signal s_ras_prediction : std_logic_vector(C_WORD_SIZE-3 downto 0);
  signal s_ras_predict_taken : std_logic;

  signal s_invalidating : std_logic;
  signal s_invalidate_adr : unsigned(C_LOG2_ENTRIES-1 downto 0);

  signal s_global_history : std_logic_vector(C_GHR_BITS-1 downto 0);

  signal s_prev_read_en : std_logic;
  signal s_prev_read_pc : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_btb_prediction : std_logic_vector(C_WORD_SIZE-3 downto 0);
  signal s_got_match : std_logic;
  signal s_got_branch_type : T_BRANCH_TYPE;
  signal s_got_taken : std_logic;

  signal s_prev_write_branch_type : T_BRANCH_TYPE;
  signal s_prev_write_is_taken : std_logic;
  signal s_prev_write_addr : std_logic_vector(C_LOG2_ENTRIES-1 downto 0);
  signal s_prev_write_tag : std_logic_vector(C_TAG_SIZE-1 downto 0);
  signal s_prev_write_target : std_logic_vector(C_WORD_SIZE-3 downto 0);

  signal s_read_addr : std_logic_vector(C_LOG2_ENTRIES-1 downto 0);
  signal s_tag_read_data : std_logic_vector(C_TAG_SIZE-1 downto 0);
  signal s_target_read_data : std_logic_vector(C_ENTRY_SIZE-1 downto 0);

  signal s_counter_read_addr : std_logic_vector(C_LOG2_ENTRIES-1 downto 0);
  signal s_counter_read_data : std_logic_vector(C_COUNTER_SIZE-1 downto 0);

  signal s_write_addr : std_logic_vector(C_LOG2_ENTRIES-1 downto 0);
  signal s_we : std_logic;
  signal s_tag_write_data : std_logic_vector(C_TAG_SIZE-1 downto 0);
  signal s_target_write_data : std_logic_vector(C_ENTRY_SIZE-1 downto 0);
  signal s_updated_counter : std_logic_vector(C_COUNTER_SIZE-1 downto 0);
  signal s_counter_predicts_taken : std_logic;
  signal s_counter_write_data : std_logic_vector(C_COUNTER_SIZE-1 downto 0);

  function table_address(pc : std_logic_vector; gh : std_logic_vector) return std_logic_vector is
  begin
    return pc(C_LOG2_ENTRIES-C_GHR_BITS+2-1 downto 2) & gh;
  end function;

  function make_tag(pc : std_logic_vector) return std_logic_vector is
  begin
    return pc(C_WORD_SIZE-1 downto C_WORD_SIZE-C_TAG_SIZE);
  end function;

  function update_counter(count : std_logic_vector; is_taken : std_logic) return std_logic_vector is
    variable v_count : unsigned(C_COUNTER_SIZE-1 downto 0);
  begin
    v_count := unsigned(count);
    if is_taken = '0' and v_count > 0 then
      v_count := v_count - 1;
    elsif is_taken = '1' and v_count < C_MAX_COUNT then
      v_count := v_count + 1;
    end if;
    return std_logic_vector(v_count);
  end function;

begin
  -- Instantiate the tag RAM.
  tag_ram_0: entity work.ram_dual_port
    generic map (
      WIDTH => C_TAG_SIZE,
      ADDR_BITS => C_LOG2_ENTRIES
    )
    port map (
      i_clk => i_clk,
      i_write_addr => s_write_addr,
      i_write_data => s_tag_write_data,
      i_we => s_we,
      i_read_addr => s_read_addr,
      o_read_data => s_tag_read_data
    );

  -- Instantiate the branch target RAM.
  target_ram_0: entity work.ram_dual_port
    generic map (
      WIDTH => C_ENTRY_SIZE,
      ADDR_BITS => C_LOG2_ENTRIES
    )
    port map (
      i_clk => i_clk,
      i_write_addr => s_write_addr,
      i_write_data => s_target_write_data,
      i_we => s_we,
      i_read_addr => s_read_addr,
      o_read_data => s_target_read_data
    );

  -- Instantiate the branch counter RAM.
  counter_ram_0: entity work.ram_dual_port
    generic map (
      WIDTH => C_COUNTER_SIZE,
      ADDR_BITS => C_LOG2_ENTRIES
    )
    port map (
      i_clk => i_clk,
      i_write_addr => s_write_addr,
      i_write_data => s_counter_write_data,
      i_we => s_we,
      i_read_addr => s_counter_read_addr,
      o_read_data => s_counter_read_data
    );

  -- Internal state.
  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_prev_read_en <= '0';
      s_prev_read_pc <= (others => '0');
    elsif rising_edge(i_clk) then
      s_prev_read_en <= i_read_en and not s_invalidating;
      s_prev_read_pc <= i_read_pc;
    end if;
  end process;


  --------------------------------------------------------------------------------------------------
  -- Invalidation state machine.
  --------------------------------------------------------------------------------------------------

  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_invalidating <= '1';
      s_invalidate_adr <= (others => '0');
    elsif rising_edge(i_clk) then
      if i_invalidate = '1' then
        s_invalidating <= '1';
        s_invalidate_adr <= (others => '0');
      elsif s_invalidating = '1' then
        if s_invalidate_adr = C_NUM_ENTRIES-1 then
          s_invalidating <= '0';
        end if;
        s_invalidate_adr <= s_invalidate_adr + 1;
      end if;
    end if;
  end process;


  --------------------------------------------------------------------------------------------------
  -- Global history.
  --------------------------------------------------------------------------------------------------

  -- TODO(m): Extend this to use speculative and non-speculative histories, similarly to how the
  -- RAS works.
  GH_GEN: if C_GHR_BITS > 0 generate
    process(i_clk, i_rst)
    begin
      if i_rst = '1' then
        s_global_history <= (others => '0');
      elsif rising_edge(i_clk) then
        if i_invalidate = '1' then
          s_global_history <= (others => '0');
        elsif i_write_branch_type /= C_BRANCH_NONE then
          s_global_history <= i_write_is_taken & s_global_history(C_GHR_BITS-1 downto 1);
        end if;
      end if;
    end process;
  end generate;


  --------------------------------------------------------------------------------------------------
  -- Branch target buffer.
  --
  -- The BTB is implemented as a direct-mapped cache containing per-instruction branch information.
  --
  --   * A BTB entry contains the following information:
  --     - A branch type tag (to be able to differentiate between different kinds of branches).
  --     - A single bit stating if the branch is predicted as taken or not taken.
  --     - The branch target address (as calculated by the branch execution logic).
  --     - An address tag (to determine BTB lookup validity).
  --
  --   * For each BTB entry there is also a saturating counter (in a separate buffer).
  --
  --   * BTB entries are updated as a response to the various i_write_* signals that come from the
  --     branch execution logic in the EX1 stage.
  --
  --   * The BTB is updated during two cycles (pipelined):
  --     1)   Read the counter from the counter RAM.
  --     2.a) Update the counter (+ or - depending on is_taken).
  --     2.b) Write the new counter to the counter RAM, and update the tag & target RAM:s.
  --------------------------------------------------------------------------------------------------

  -- BTB lookup.

  s_read_addr <= table_address(i_read_pc, s_global_history);

  -- Decode the branch & target information.
  s_got_branch_type <= s_target_read_data(C_ENTRY_SIZE-1 downto C_ENTRY_SIZE-2);
  s_got_taken <= s_target_read_data(C_ENTRY_SIZE-3);
  s_btb_prediction <= s_target_read_data(C_ENTRY_SIZE-4 downto 0);

  -- Check the tag: Did we have a match?
  s_got_match <= '1' when make_tag(s_prev_read_pc) = s_tag_read_data else '0';


  -- BTB update.

  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_prev_write_branch_type <= C_BRANCH_NONE;
      s_prev_write_is_taken <= '0';
      s_prev_write_addr <= (others => '0');
      s_prev_write_tag <= (others => '0');
      s_prev_write_target <= (others => '0');
    elsif rising_edge(i_clk) then
      s_prev_write_branch_type <= i_write_branch_type;
      s_prev_write_is_taken <= i_write_is_taken;
      s_prev_write_addr <= s_counter_read_addr;
      s_prev_write_tag <= make_tag(i_write_pc);
      s_prev_write_target <= i_write_target(C_WORD_SIZE-1 downto 2);
    end if;
  end process;

  -- In the first cycle, read from the counter RAM.
  s_counter_read_addr <= table_address(i_write_pc, s_global_history);

  -- Update the counter, and determine if the next prediction should be taken or not.
  s_updated_counter <= update_counter(s_counter_read_data, s_prev_write_is_taken);
  s_counter_predicts_taken <= s_updated_counter(C_COUNTER_SIZE-1);  -- MSB of counter = predict taken

  -- Prepare write operations to the different RAM:s.
  s_we <= '1' when s_invalidating = '1' or s_prev_write_branch_type /= C_BRANCH_NONE else '0';
  s_write_addr <= std_logic_vector(s_invalidate_adr) when s_invalidating = '1' else
                  s_prev_write_addr;
  s_tag_write_data <= (others => '0') when s_invalidating = '1' else
                      s_prev_write_tag;
  s_target_write_data <= (others => '0') when s_invalidating = '1' else
                         s_prev_write_branch_type & s_counter_predicts_taken & s_prev_write_target;
  s_counter_write_data <= (C_COUNTER_SIZE-1 => '0', others => '1') when s_invalidating = '1' else
                          s_updated_counter;


  --------------------------------------------------------------------------------------------------
  -- Return address stack.
  --
  -- The RAS is implemented as a circular buffer containing up to N different return addresses at
  -- different call depths, and helps with predicting the return address for function calls (e.g.
  -- malloc() may be called from many different callers, which makes it hard or impossible for the
  -- BTB to predict the branch target of a RET instruction).
  --
  --   * When a CALL branch is encountered (i.e. any "JL" instruction), PC+4 is pushed onto the
  --     return address stack (the stack index is increased).
  --
  --   * When a RET branch is encountered (i.e. a "J LR,#0" instruction), the return address is
  --     popped from the return address stack (the stack index is decreased) and used as the
  --     predicted branch target address.
  --
  --   * There are two stack indices:
  --     - There is a speciulative stack index, which is controlled by the branch type that is
  --       fetched from the BTB buffer, and thus the speculative stack index is updated in sync
  --       with the instruction fetch. The speculative stack index is used for RAS predictions.
  --
  --     - Additionally there is a "correct" (non-speculative) stack index that is controlled by
  --       the branch logic in the EX1 stage, and thus the non-speculative stack index lags behind
  --       the instruciton fetch by a few cycles.
  --
  --     - When a branch misprediction is detected in EX1, the speculative stack index is rewound
  --       to the non-speculative stack index.
  --
  --   * For deep call chains, the circular buffer may overflow and thus the oldest entries will
  --     silently be overwritten. However, the N most recent return addresses will still be
  --     correctly predicted (which should provide accurate prediction for most performance
  --     sensitive code that loops around a call tree that is <N deep). This strategy also works
  --     well for many recursive algorithms, since the stack will be filled with a single return
  --     address, and thus as overwritten entries are used for prediction, they will just happen to
  --     be correct.
  --------------------------------------------------------------------------------------------------

  RAS_GEN: if C_LOG2_RAS_ENTRIES > 0 generate
    process(i_clk, i_rst)
      variable v_branch_type : T_BRANCH_TYPE;
      variable v_ras_index : unsigned(C_LOG2_RAS_ENTRIES-1 downto 0);
      variable v_return_pc : unsigned(C_WORD_SIZE-3 downto 0);
      variable v_write_addr : unsigned(C_LOG2_RAS_ENTRIES-1 downto 0);
    begin
      if i_rst = '1' then
        s_ras_index <= (others => '0');
        s_ras_correct_index <= (others => '0');
      elsif rising_edge(i_clk) then
        -- Get current RAS index & the type of the current branch (if any).
        if s_invalidating = '1' then
          -- Reset the RAS index when the branch predictor is invalidated.
          v_branch_type := C_BRANCH_NONE;
          v_ras_index := to_unsigned(0, C_LOG2_RAS_ENTRIES);
          v_return_pc := to_unsigned(1, C_WORD_SIZE-2);
        elsif i_cancel_speculation = '1' then
          -- Rewind the speculative RAS index to the correct one when we get a request to cancel
          -- speculative state. This happens when the branch logic detects a branch misprediction.
          v_branch_type := i_write_branch_type;
          v_ras_index := s_ras_correct_index;
          v_return_pc := unsigned(i_write_pc(C_WORD_SIZE-1 downto 2)) + 1;
        else
          -- ...otherwise (during "normal operation"), we base the RAS index on the speculative
          -- state (i.e. the most recent outcome of branch prediction).
          v_branch_type := s_got_branch_type;
          v_ras_index := s_ras_index;
          v_return_pc := unsigned(s_prev_read_pc(C_WORD_SIZE-1 downto 2)) + 1;
        end if;

        -- Calculate the next RAS index based on branch type.
        if v_branch_type = C_BRANCH_CALL then
          v_ras_index := v_ras_index + 1;
        elsif v_branch_type = C_BRANCH_RET then
          v_ras_index := v_ras_index - 1;
        end if;

        -- Push the return address onto the return address stack.
        if s_invalidating = '1' or v_branch_type = C_BRANCH_CALL then
          if s_invalidating = '1' then
            v_write_addr := s_invalidate_adr(C_LOG2_RAS_ENTRIES-1 downto 0);
          else
            v_write_addr := v_ras_index;
          end if;
          s_ras_array(to_integer(v_write_addr)) <= std_logic_vector(v_return_pc);
        end if;

        -- Update the speculative RAS index.
        s_ras_index <= v_ras_index;

        -- Update the "correct" RAS index, based on branch commands from the EX1 stage.
        -- Note that this is lagging behind the speculative state due to pipelining.
        if s_invalidating = '1' then
          s_ras_correct_index <= to_unsigned(0, C_LOG2_RAS_ENTRIES);
        elsif i_write_branch_type = C_BRANCH_CALL then
          s_ras_correct_index <= s_ras_correct_index + 1;
        elsif i_write_branch_type = C_BRANCH_RET then
          s_ras_correct_index <= s_ras_correct_index - 1;
        end if;
      end if;
    end process;

    -- Do we have a prediction from the RAS?
    -- We read the target address from the RAS array asynchronously (write pass-through) in order to
    -- have the value ready in time during the prediction cycle.
    s_ras_prediction <= s_ras_array(to_integer(s_ras_index));
    s_ras_predict_taken <= (not s_invalidating) when s_got_branch_type = C_BRANCH_RET else '0';
  else generate
    s_ras_prediction <= (others => '0');
    s_ras_predict_taken <= '0';
  end generate;


  --------------------------------------------------------------------------------------------------
  -- The branch predictor collects information from different predictors and outputs:
  --
  --  * Whether or not the instruction was a branch that should be taken.
  --  * The target address of the branch.
  --------------------------------------------------------------------------------------------------

  o_predict_taken <= s_prev_read_en and s_got_match and (s_got_taken or s_ras_predict_taken);
  o_predict_target <= s_ras_prediction & "00" when s_ras_predict_taken = '1' else
                      s_btb_prediction & "00";

end rtl;
