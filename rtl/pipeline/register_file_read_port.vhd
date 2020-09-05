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

entity register_file_read_port is
  generic (
    ENABLE_VECTOR_REGISTERS : boolean
  );
  port (
    i_clk : in std_logic;
    i_rst : in std_logic;
    i_stall : in std_logic;

    -- Asynchronous read request.
    i_rd_port : in T_SRC_REG;

    -- Output read data.
    o_data : out std_logic_vector(C_WORD_SIZE-1 downto 0);

    -- We have one write port.
    i_wr_port : in T_DST_REG;
    i_data_w : in std_logic_vector(C_WORD_SIZE-1 downto 0);

    -- The PC register always returns the current PC.
    i_pc : in std_logic_vector(C_WORD_SIZE-1 downto 0)
  );
end register_file_read_port;

architecture rtl of register_file_read_port is
  constant C_SCALAR_ADDR_BITS : positive := C_LOG2_NUM_REGS;
  constant C_VECTOR_ADDR_BITS : positive := C_LOG2_NUM_REGS + C_LOG2_VEC_REG_ELEMENTS;

  signal s_rd_port : T_SRC_REG;
  signal s_prev_rd_port : T_SRC_REG;

  signal s_req_z : std_logic;
  signal s_req_pc : std_logic;
  signal s_prev_req_z : std_logic;
  signal s_prev_req_pc : std_logic;

  signal s_scalar_read_addr : std_logic_vector(C_SCALAR_ADDR_BITS-1 downto 0);
  signal s_scalar_write_addr : std_logic_vector(C_SCALAR_ADDR_BITS-1 downto 0);
  signal s_scalar_we : std_logic;
  signal s_scalar_read_data : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_vector_read_data : std_logic_vector(C_WORD_SIZE-1 downto 0);

  function is_zero_reg(src_reg : T_SRC_REG) return std_logic is
  begin
    if src_reg.reg = to_vector(C_Z_REG, C_LOG2_NUM_REGS) then
      return '1';
    else
      return '0';
    end if;
  end function;

  function is_pc_reg(src_reg : T_SRC_REG) return std_logic is
  begin
    if src_reg.reg = to_vector(C_PC_REG, C_LOG2_NUM_REGS) and src_reg.is_vector = '0' then
      return '1';
    else
      return '0';
    end if;
  end function;

begin
  -- Latch the read operations.
  process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      s_prev_rd_port.reg <= (others => '0');
      s_prev_rd_port.element <= (others => '0');
      s_prev_rd_port.is_vector <= '0';
      s_prev_req_z <= '0';
      s_prev_req_pc <= '0';
    elsif rising_edge(i_clk) then
      if i_stall = '0' then
        s_prev_rd_port <= s_rd_port;
        s_prev_req_z <= s_req_z;
        s_prev_req_pc <= s_req_pc;
      end if;
    end if;
  end process;

  -- Handle stall: Use inputs or latched inputs from the previous cycle.
  s_rd_port <= s_prev_rd_port when i_stall = '1' else i_rd_port;
  s_req_z <= s_prev_req_z when i_stall = '1' else is_zero_reg(i_rd_port);
  s_req_pc <= s_prev_req_pc when i_stall = '1' else is_pc_reg(i_rd_port);

  -- Prepare scalar read & write signals.
  s_scalar_read_addr <= s_rd_port.reg;
  s_scalar_write_addr <= i_wr_port.reg;
  s_scalar_we <= i_wr_port.is_target and not i_wr_port.is_vector;

  -- Scalar registers RAM.
  scalar_ram_1: entity work.ram_dual_port
    generic map (
      WIDTH => C_WORD_SIZE,
      ADDR_BITS => C_SCALAR_ADDR_BITS,
      PREFER_DISTRIBUTED => true
    )
    port map (
      i_clk => i_clk,
      i_write_data => i_data_w,
      i_write_addr => s_scalar_write_addr,
      i_we => s_scalar_we,
      i_read_addr => s_scalar_read_addr,
      o_read_data => s_scalar_read_data
    );

  vreg_gen: if ENABLE_VECTOR_REGISTERS generate
    signal s_vector_read_addr : std_logic_vector(C_VECTOR_ADDR_BITS-1 downto 0);
    signal s_vector_write_addr : std_logic_vector(C_VECTOR_ADDR_BITS-1 downto 0);
    signal s_vector_we : std_logic;
  begin
    -- Prepare vector read & write signals.
    s_vector_read_addr <= s_rd_port.reg & s_rd_port.element;
    s_vector_write_addr <= i_wr_port.reg & i_wr_port.element;
    s_vector_we <= i_wr_port.is_target and i_wr_port.is_vector;

    -- Vector registers RAM.
    vector_ram_1: entity work.ram_dual_port
      generic map (
        WIDTH => C_WORD_SIZE,
        ADDR_BITS => C_VECTOR_ADDR_BITS
      )
      port map (
        i_clk => i_clk,
        i_write_data => i_data_w,
        i_write_addr => s_vector_write_addr,
        i_we => s_vector_we,
        i_read_addr => s_vector_read_addr,
        o_read_data => s_vector_read_data
      );
  else generate
    s_vector_read_data <= (others => '0');
  end generate;

  -- Output signal.
  o_data <= (others => '0') when s_prev_req_z = '1' else
            i_pc when s_prev_req_pc = '1' else
            s_scalar_read_data when s_prev_rd_port.is_vector = '0' else
            s_vector_read_data;
end rtl;
