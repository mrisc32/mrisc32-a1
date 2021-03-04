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
-- This entity implements bit field and shifting instructions.
-- TODO(m): Optimize this, as it currently uses up quite a lot of logic.
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.config.all;
use work.types.all;

entity shift32 is
  generic(
    CONFIG : T_CORE_CONFIG
  );
  port(
    i_right       : in  std_logic;  -- '1' for right shifts, '0' for left
    i_arithmetic  : in  std_logic;  -- '1' for arihtmetic shifts, '0' for logic
    i_src         : in  std_logic_vector(31 downto 0);
    i_ctrl        : in  std_logic_vector(31 downto 0);
    i_packed_mode : in  T_PACKED_MODE;
    o_result      : out std_logic_vector(31 downto 0)
  );
end shift32;

architecture rtl of shift32 is
  -- Mask LUT for 32-bit values.
  type LUT32_T is array (natural range 0 to 31) of std_logic_vector(31 downto 0);
  constant C_LUT32 : LUT32_T := (
    "00000000000000000000000000000001", "00000000000000000000000000000011",
    "00000000000000000000000000000111", "00000000000000000000000000001111",
    "00000000000000000000000000011111", "00000000000000000000000000111111",
    "00000000000000000000000001111111", "00000000000000000000000011111111",
    "00000000000000000000000111111111", "00000000000000000000001111111111",
    "00000000000000000000011111111111", "00000000000000000000111111111111",
    "00000000000000000001111111111111", "00000000000000000011111111111111",
    "00000000000000000111111111111111", "00000000000000001111111111111111",
    "00000000000000011111111111111111", "00000000000000111111111111111111",
    "00000000000001111111111111111111", "00000000000011111111111111111111",
    "00000000000111111111111111111111", "00000000001111111111111111111111",
    "00000000011111111111111111111111", "00000000111111111111111111111111",
    "00000001111111111111111111111111", "00000011111111111111111111111111",
    "00000111111111111111111111111111", "00001111111111111111111111111111",
    "00011111111111111111111111111111", "00111111111111111111111111111111",
    "01111111111111111111111111111111", "11111111111111111111111111111111"
  );

  -- Mask LUT for 16-bit values.
  type LUT16_T is array (natural range 0 to 15) of std_logic_vector(15 downto 0);
  constant C_LUT16 : LUT16_T := (
    "0000000000000001", "0000000000000011", "0000000000000111", "0000000000001111",
    "0000000000011111", "0000000000111111", "0000000001111111", "0000000011111111",
    "0000000111111111", "0000001111111111", "0000011111111111", "0000111111111111",
    "0001111111111111", "0011111111111111", "0111111111111111", "1111111111111111"
  );

  -- Mask LUT for 8-bit values.
  type LUT8_T is array (natural range 0 to 7) of std_logic_vector(7 downto 0);
  constant C_LUT8 : LUT8_T := (
    "00000001", "00000011", "00000111", "00001111", "00011111", "00111111", "01111111", "11111111"
  );

  function decode_offset(ctrl : std_logic_vector; bits : natural) return natural is
  begin
    return to_integer(unsigned(ctrl(ctrl'right+bits-1 downto ctrl'right)));
  end function;

  function decode_width(ctrl : std_logic_vector; bits : natural) return positive is
    variable v_width : natural;
  begin
    v_width := to_integer(unsigned(ctrl(ctrl'right+bits*2-1 downto ctrl'right+bits)));
    if v_width = 0 then
      return 2**bits;
    end if;
    return v_width;
  end function;

  signal s_result8 : std_logic_vector(31 downto 0);
  signal s_result16 : std_logic_vector(31 downto 0);
  signal s_result32 : std_logic_vector(31 downto 0);
begin
  -- Word operation.
  process(i_right, i_arithmetic, i_src, i_ctrl)
    variable v_offset : natural;
    variable v_width : positive;
    variable v_mask_width : positive;
    variable v_sign_bit_idx : natural;
    variable v_mask : std_logic_vector(31 downto 0);
    variable v_shifted : std_logic_vector(31 downto 0);
  begin
    -- Decode offset and width.
    v_offset := decode_offset(i_ctrl, 5);
    v_width := decode_width(i_ctrl, 5);
    if (v_offset + v_width) > 32 then
      v_width := 32 - v_offset;
    end if;

    -- Shift.
    if i_right = '1' then
      v_shifted := std_logic_vector(shift_right(unsigned(i_src), v_offset));
    else
      v_shifted := std_logic_vector(shift_left(unsigned(i_src), v_offset));
    end if;

    -- Get mask.
    if i_right = '1' then
      v_mask_width := v_width;
    else
      v_mask_width := v_width + v_offset;
    end if;
    v_mask := C_LUT32(v_mask_width - 1);

    -- Handle sign. The final result = (shifted & mask) | (sign & ~mask)
    v_sign_bit_idx := v_width + v_offset - 1;
    if i_arithmetic = '1' and i_src(v_sign_bit_idx) = '1' then
      s_result32 <= (v_shifted and v_mask) or (not v_mask);
    else
      s_result32 <= (v_shifted and v_mask);
    end if;
  end process;

  PACKED_GEN: if CONFIG.HAS_PO generate
    -- Packed byte operation.
    process(i_right, i_arithmetic, i_src, i_ctrl)
      variable v_lo : natural;
      variable v_hi : natural;
      variable v_offset : natural;
      variable v_width : positive;
      variable v_mask_width : positive;
      variable v_sign_bit_idx : natural;
      variable v_mask : std_logic_vector(7 downto 0);
      variable v_shifted : std_logic_vector(7 downto 0);
    begin
      for k in 0 to 3 loop
        v_lo := k * 8;
        v_hi := v_lo + 7;

        -- Decode offset and width.
        v_offset := decode_offset(i_ctrl(v_hi downto v_lo), 3);
        v_width := decode_width(i_ctrl(v_hi downto v_lo), 3);
        if (v_offset + v_width) > 8 then
          v_width := 8 - v_offset;
        end if;

        -- Shift.
        if i_right = '1' then
          v_shifted := std_logic_vector(shift_right(unsigned(i_src(v_hi downto v_lo)), v_offset));
        else
          v_shifted := std_logic_vector(shift_left(unsigned(i_src(v_hi downto v_lo)), v_offset));
        end if;

        -- Get mask.
        if i_right = '1' then
          v_mask_width := v_width;
        else
          v_mask_width := v_width + v_offset;
        end if;
        v_mask := C_LUT8(v_mask_width - 1);

        -- Handle sign. The final result = (shifted & mask) | (sign & ~mask)
        v_sign_bit_idx := v_width + v_offset - 1;
        if i_arithmetic = '1' and i_src(v_sign_bit_idx + v_lo) = '1' then
          s_result8(v_hi downto v_lo) <= (v_shifted and v_mask) or (not v_mask);
        else
          s_result8(v_hi downto v_lo) <= (v_shifted and v_mask);
        end if;
      end loop;
    end process;

    -- Packed half-word operation.
    process(i_right, i_arithmetic, i_src, i_ctrl)
      variable v_lo : natural;
      variable v_hi : natural;
      variable v_offset : natural;
      variable v_width : positive;
      variable v_mask_width : positive;
      variable v_sign_bit_idx : natural;
      variable v_mask : std_logic_vector(15 downto 0);
      variable v_shifted : std_logic_vector(15 downto 0);
    begin
      for k in 0 to 1 loop
        v_lo := k * 16;
        v_hi := v_lo + 15;

        -- Decode offset and width.
        v_offset := decode_offset(i_ctrl(v_hi downto v_lo), 4);
        v_width := decode_width(i_ctrl(v_hi downto v_lo), 4);
        if (v_offset + v_width) > 16 then
          v_width := 16 - v_offset;
        end if;

        -- Shift.
        if i_right = '1' then
          v_shifted := std_logic_vector(shift_right(unsigned(i_src(v_hi downto v_lo)), v_offset));
        else
          v_shifted := std_logic_vector(shift_left(unsigned(i_src(v_hi downto v_lo)), v_offset));
        end if;

        -- Get mask.
        if i_right = '1' then
          v_mask_width := v_width;
        else
          v_mask_width := v_width + v_offset;
        end if;
        v_mask := C_LUT16(v_mask_width - 1);

        -- Handle sign. The final result = (shifted & mask) | (sign & ~mask)
        v_sign_bit_idx := v_width + v_offset - 1;
        if i_arithmetic = '1' and i_src(v_sign_bit_idx + v_lo) = '1' then
          s_result16(v_hi downto v_lo) <= (v_shifted and v_mask) or (not v_mask);
        else
          s_result16(v_hi downto v_lo) <= (v_shifted and v_mask);
        end if;
      end loop;
    end process;

    -- Select the appropriate result based on the packed mode.
    o_result <= s_result8  when i_packed_mode = C_PACKED_BYTE else
                s_result16 when i_packed_mode = C_PACKED_HALF_WORD else
                s_result32;
  else generate
    -- In unpacked mode we only have to consider the 32-bit result.
    o_result <= s_result32;
  end generate;
end rtl;
