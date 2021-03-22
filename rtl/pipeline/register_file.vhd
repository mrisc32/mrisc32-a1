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
use work.config.all;

---------------------------------------------------------------------------------------------------
-- This implements the scalar and vector register files, with the following properties:
--
--  * There are three generic read ports.
--  * There is a single write port.
--  * Reading the Z or VZ registers always returns zero (0).
--  * Writing to the Z or VZ registers has no effect (no operation).
--  * Register content is undefined after reset.
---------------------------------------------------------------------------------------------------

entity register_file is
  generic (
    ENABLE_VECTOR_REGISTERS : boolean
  );
  port (
    i_clk : in std_logic;
    i_rst : in std_logic;
    i_stall_read_ports : in std_logic;

    -- Asynchronous read requestes (three read ports).
    i_rd_port_a : in T_SRC_REG;
    i_rd_port_b : in T_SRC_REG;
    i_rd_port_c : in T_SRC_REG;

    -- Output read data.
    o_data_a : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_data_b : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_data_c : out std_logic_vector(C_WORD_SIZE-1 downto 0);

    -- We have one write port.
    i_wr_port : in T_DST_REG;
    i_data_w : in std_logic_vector(C_WORD_SIZE-1 downto 0)
  );
end register_file;

architecture rtl of register_file is
begin
  -- Read port A.
  read_port_a: entity work.register_file_read_port
    generic map (
      ENABLE_VECTOR_REGISTERS => ENABLE_VECTOR_REGISTERS
    )
    port map (
      i_clk => i_clk,
      i_rst => i_rst,
      i_stall => i_stall_read_ports,
      i_rd_port => i_rd_port_a,
      o_data => o_data_a,
      i_wr_port => i_wr_port,
      i_data_w => i_data_w
    );

  -- Read port B.
  read_port_b: entity work.register_file_read_port
    generic map (
      ENABLE_VECTOR_REGISTERS => ENABLE_VECTOR_REGISTERS
    )
    port map (
      i_clk => i_clk,
      i_rst => i_rst,
      i_stall => i_stall_read_ports,
      i_rd_port => i_rd_port_b,
      o_data => o_data_b,
      i_wr_port => i_wr_port,
      i_data_w => i_data_w
    );

  -- Read port C.
  read_port_c: entity work.register_file_read_port
    generic map (
      ENABLE_VECTOR_REGISTERS => ENABLE_VECTOR_REGISTERS
    )
    port map (
      i_clk => i_clk,
      i_rst => i_rst,
      i_stall => i_stall_read_ports,
      i_rd_port => i_rd_port_c,
      o_data => o_data_c,
      i_wr_port => i_wr_port,
      i_data_w => i_data_w
    );
end rtl;
