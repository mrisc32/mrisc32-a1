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

----------------------------------------------------------------------------------------------------
-- This is a configurable FPACK pipeline. The pipeline can be instantiated for different sizes
-- (32-bit and 16-bit floating-point).
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.types.all;

entity fpack is
  generic(
    WIDTH : positive := 32;
    EXP_BITS : positive := 8;
    EXP_BIAS : positive := 127;
    FRACT_BITS : positive := 23;

    PACKED_WIDTH : positive := 16;
    PACKED_EXP_BITS : positive := 5;
    PACKED_EXP_BIAS : positive := 15;
    PACKED_FRACT_BITS : positive := 10
  );
  port(
    -- Control signals.
    i_clk : in std_logic;
    i_rst : in std_logic;
    i_stall : in std_logic;

    -- Inputs (async).
    i_enable : in std_logic;

    i_props_a : in T_FLOAT_PROPS;
    i_exponent_a : in std_logic_vector(EXP_BITS-1 downto 0);
    i_significand_a : in std_logic_vector(FRACT_BITS downto 0);

    i_props_b : in T_FLOAT_PROPS;
    i_exponent_b : in std_logic_vector(EXP_BITS-1 downto 0);
    i_significand_b : in std_logic_vector(FRACT_BITS downto 0);

    -- Outputs (async).
    o_result : out std_logic_vector(WIDTH-1 downto 0);
    o_result_ready : out std_logic
  );
end fpack;

architecture rtl of fpack is
  -- Constants.
  constant SIGNIFICAND_BITS : positive := FRACT_BITS + 1;
  constant PACKED_SIGNIFICAND_BITS : positive := PACKED_FRACT_BITS + 1;

  -- Types.
  type T_PACKED_FLOAT is record
    props : T_FLOAT_PROPS;
    exponent : std_logic_vector(PACKED_EXP_BITS-1 downto 0);
    significand : std_logic_vector(PACKED_SIGNIFICAND_BITS-1 downto 0);
  end record T_PACKED_FLOAT;

  -- F1 signals.
  signal s_f1_enable : std_logic;
  signal s_f1_packed_a : T_PACKED_FLOAT;
  signal s_f1_packed_b : T_PACKED_FLOAT;

  -- F2 signals.
  signal s_f2_next_result_a : std_logic_vector(PACKED_WIDTH-1 downto 0);
  signal s_f2_next_result_b : std_logic_vector(PACKED_WIDTH-1 downto 0);

  function pack_float(props : T_FLOAT_PROPS;
                      exponent : std_logic_vector(EXP_BITS-1 downto 0);
                      significand : std_logic_vector(FRACT_BITS downto 0)) return T_PACKED_FLOAT is
    variable v_result : T_PACKED_FLOAT;
    variable v_significand_rounded : unsigned(FRACT_BITS+1 downto 0);
    variable v_exponent_needs_adjust : std_logic;
    variable v_exponent_adjust : unsigned(EXP_BITS downto 0);
    variable v_adjusted_exponent : unsigned(EXP_BITS downto 0);
    variable v_overflow : std_logic;
    variable v_underflow : std_logic;
  begin
    -- Round the significand.
    v_significand_rounded := unsigned('0' & significand) + 2**(FRACT_BITS - PACKED_FRACT_BITS - 1);
    v_exponent_needs_adjust := v_significand_rounded(FRACT_BITS+1);
    v_exponent_adjust := to_unsigned(0, EXP_BITS) & v_exponent_needs_adjust;
    if v_exponent_needs_adjust = '1' then
      v_result.significand := std_logic_vector(v_significand_rounded(FRACT_BITS+1 downto (FRACT_BITS-PACKED_FRACT_BITS+1)));
    else
      v_result.significand := std_logic_vector(v_significand_rounded(FRACT_BITS downto (FRACT_BITS-PACKED_FRACT_BITS)));
    end if;

    -- Adjust the exponent.
    v_adjusted_exponent := (unsigned('0' & exponent) - EXP_BIAS + PACKED_EXP_BIAS) + v_exponent_adjust;
    v_result.exponent := std_logic_vector(v_adjusted_exponent(PACKED_EXP_BITS-1 downto 0));

    -- Check for underflow.
    v_underflow := v_adjusted_exponent(EXP_BITS);

    -- Check for overflow.
    if v_adjusted_exponent(EXP_BITS-1 downto PACKED_EXP_BITS) /= 0 then
      v_overflow := not v_underflow;
    else
      v_overflow := '0';
    end if;

    -- Determine float properties.
    v_result.props.is_neg := props.is_neg;
    v_result.props.is_nan := props.is_nan;
    v_result.props.is_inf := props.is_inf or v_overflow;
    v_result.props.is_zero := props.is_zero or v_underflow;

    return v_result;
  end function;

begin
  --================================================================================================
  -- F1: Stage 1 of the pipeline.
  --================================================================================================

  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_f1_enable <= '0';
      s_f1_packed_a <= (('0', '0', '0', '0'), (others => '0'), (others => '0'));
      s_f1_packed_b <= (('0', '0', '0', '0'), (others => '0'), (others => '0'));
    elsif rising_edge(i_clk) then
      if i_stall = '0' then
        s_f1_enable <= i_enable;
        s_f1_packed_a <= pack_float(i_props_a, i_exponent_a, i_significand_a);
        s_f1_packed_b <= pack_float(i_props_b, i_exponent_b, i_significand_b);
      end if;
    end if;
  end process;


  --==================================================================================================
  -- F2: Stage 2 of the pipeline.
  --==================================================================================================

  -- Compose the two packed floating-point values.
  ComposeA: entity work.float_compose
    generic map (
      WIDTH => PACKED_WIDTH,
      EXP_BITS => PACKED_EXP_BITS,
      FRACT_BITS => PACKED_FRACT_BITS
    )
    port map (
      i_props => s_f1_packed_a.props,
      i_exponent => s_f1_packed_a.exponent,
      i_significand => s_f1_packed_a.significand,
      o_result => s_f2_next_result_a
    );

  ComposeB: entity work.float_compose
    generic map (
      WIDTH => PACKED_WIDTH,
      EXP_BITS => PACKED_EXP_BITS,
      FRACT_BITS => PACKED_FRACT_BITS
    )
    port map (
      i_props => s_f1_packed_b.props,
      i_exponent => s_f1_packed_b.exponent,
      i_significand => s_f1_packed_b.significand,
      o_result => s_f2_next_result_b
    );

  -- Output the result.
  o_result <= s_f2_next_result_a & s_f2_next_result_b;
  o_result_ready <= s_f1_enable;
end rtl;
