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
use work.types.all;
use work.config.all;

entity pack32 is
  generic(
    CONFIG : T_CORE_CONFIG
  );
  port(
    i_src_a       : in std_logic_vector(31 downto 0);
    i_src_b       : in std_logic_vector(31 downto 0);
    i_saturate    : in std_logic;
    i_unsigned    : in std_logic;
    i_packed_mode : in T_PACKED_MODE;
    o_result      : out std_logic_vector(31 downto 0)
  );
end pack32;

architecture rtl of pack32 is
  signal s_res_32 : std_logic_vector(31 downto 0);
  signal s_res_16 : std_logic_vector(31 downto 0);
  signal s_res_8 : std_logic_vector(31 downto 0);

  function pack(a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    variable v_hi : std_logic_vector((a'length / 2)-1 downto 0);
    variable v_lo : std_logic_vector((b'length / 2)-1 downto 0);
  begin
    v_hi := a((a'left + a'right)/2 downto a'right);
    v_lo := b((b'left + b'right)/2 downto b'right);
    return v_hi & v_lo;
  end function;
begin
  -- 32-bit pack.
  s_res_32 <= pack(i_src_a, i_src_b);

  PACKED_GEN: if CONFIG.HAS_PO generate
    -- 2x 16-bit pack.
    s_res_16(15 downto 0) <= pack(i_src_a(15 downto 0), i_src_b(15 downto 0));
    s_res_16(31 downto 16) <= pack(i_src_a(31 downto 16), i_src_b(31 downto 16));

    -- 4x 8-bit pack.
    s_res_8(7 downto 0) <= pack(i_src_a(7 downto 0), i_src_b(7 downto 0));
    s_res_8(15 downto 8) <= pack(i_src_a(15 downto 8), i_src_b(15 downto 8));
    s_res_8(23 downto 16) <= pack(i_src_a(23 downto 16), i_src_b(23 downto 16));
    s_res_8(31 downto 24) <= pack(i_src_a(31 downto 24), i_src_b(31 downto 24));

    -- Select the result based on the packed mode.
    o_result <= s_res_8  when i_packed_mode = C_PACKED_BYTE else
                s_res_16 when i_packed_mode = C_PACKED_HALF_WORD else
                s_res_32;
  else generate
    -- In unpacked mode we only have to consider the 32-bit result.
    o_result <= s_res_32;
  end generate;
end rtl;
