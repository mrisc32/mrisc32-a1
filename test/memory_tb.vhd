----------------------------------------------------------------------------------------------------
-- Copyright (c) 2019 Marcus Geelnard
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

entity memory_tb is
end memory_tb;

architecture behavioral of memory_tb is
  signal s_clk : std_logic;
  signal s_rst : std_logic;
  signal s_stall : std_logic;
  signal s_stall_out : std_logic;

  signal s_mem_enable : std_logic;
  signal s_mem_op : T_MEM_OP;
  signal s_mem_adr : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_mem_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);

  signal s_cache_cyc : std_logic;
  signal s_cache_stb : std_logic;
  signal s_cache_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_cache_we : std_logic;
  signal s_cache_sel : std_logic_vector(C_WORD_SIZE/8-1 downto 0);
  signal s_cache_dat_out : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_cache_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_cache_ack : std_logic;
  signal s_cache_stall : std_logic;
  signal s_cache_err : std_logic;

  signal s_result : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_result_ready : std_logic;
begin
  memory_0: entity work.memory
    port map (
      i_clk => s_clk,
      i_rst => s_rst,
      i_stall => s_stall,
      o_stall => s_stall_out,

      i_mem_enable => s_mem_enable,
      i_mem_op => s_mem_op,
      i_mem_adr => s_mem_adr,
      i_mem_dat => s_mem_dat,

      o_cache_cyc => s_cache_cyc,
      o_cache_stb => s_cache_stb,
      o_cache_adr => s_cache_adr,
      o_cache_we => s_cache_we,
      o_cache_sel => s_cache_sel,
      o_cache_dat => s_cache_dat_out,
      i_cache_dat => s_cache_dat,
      i_cache_ack => s_cache_ack,
      i_cache_stall => s_cache_stall,
      i_cache_err => s_cache_err,

      o_result => s_result,
      o_result_ready => s_result_ready
    );

  process
    -- Patterns to apply.
    type pattern_type is record
      -- Inputs
      stall : std_logic;

      mem_enable : std_logic;
      mem_op : T_MEM_OP;
      mem_adr : std_logic_vector(C_WORD_SIZE-1 downto 0);
      mem_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);

      cache_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
      cache_ack : std_logic;
      cache_stall : std_logic;
      cache_err : std_logic;

      -- Expected outputs
      cache_cyc : std_logic;
      cache_stb : std_logic;
      cache_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
      cache_we : std_logic;
      cache_sel : std_logic_vector(C_WORD_SIZE/8-1 downto 0);
      cache_dat_out : std_logic_vector(C_WORD_SIZE-1 downto 0);

      result : std_logic_vector(C_WORD_SIZE-1 downto 0);
      result_ready : std_logic;
      stall_out : std_logic;
    end record;
    type pattern_array is array (natural range <>) of pattern_type;
    constant patterns : pattern_array := (
        -- ===[ Inputs ]=====================================================  ===[ Outputs ]============================================

        -- No-op.
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000000",'0','0','0',  '0','0',30x"00",'0',x"0",x"00000000", x"00000000",'0','0'),
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000000",'0','0','0',  '0','0',30x"00",'0',x"0",x"00000000", x"00000000",'0','0'),
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000000",'0','0','0',  '0','0',30x"00",'0',x"0",x"00000000", x"00000000",'0','0'),

        -- Pipelined 32-bit memory load and store.
        ('0', '1',C_MEM_OP_LOAD32,  32x"24",32x"00", x"00000000",'0','0','0',  '1','1',30x"09",'0',x"f",x"00000000", x"00000000",'0','0'),
        ('0', '1',C_MEM_OP_STORE32, 32x"28",32x"21", x"00000012",'1','0','0',  '1','1',30x"0a",'1',x"f",x"00000021", x"00000012",'1','0'),
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000000",'1','0','0',  '1','0',30x"00",'0',x"0",x"00000000", x"00000000",'0','0'),

        -- Pipelined 16-bit memory load and store.
        ('0', '1',C_MEM_OP_LOADU16, 32x"24",32x"00", x"00000000",'0','0','0',  '1','1',30x"09",'0',x"3",x"00000000", x"00000000",'0','0'),
        ('0', '1',C_MEM_OP_LOADU16, 32x"26",32x"00", x"00348012",'1','0','0',  '1','1',30x"09",'0',x"c",x"00000000", x"00008012",'1','0'),
        ('0', '1',C_MEM_OP_LOAD16,  32x"24",32x"00", x"00348012",'1','0','0',  '1','1',30x"09",'0',x"3",x"00000000", x"00000034",'1','0'),
        ('0', '1',C_MEM_OP_LOAD16,  32x"26",32x"00", x"00348012",'1','0','0',  '1','1',30x"09",'0',x"c",x"00000000", x"ffff8012",'1','0'),
        ('0', '1',C_MEM_OP_STORE16, 32x"28",32x"43", x"00348012",'1','0','0',  '1','1',30x"0a",'1',x"3",x"00000043", x"00000034",'1','0'),
        ('0', '1',C_MEM_OP_STORE16, 32x"2a",32x"65", x"00000000",'1','0','0',  '1','1',30x"0a",'1',x"c",x"00650000", x"00000000",'0','0'),
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000000",'1','0','0',  '1','0',30x"00",'0',x"0",x"00000000", x"00000000",'0','0'),

        -- Pipelined 8-bit memory load and store.
        ('0', '1',C_MEM_OP_LOADU8,  32x"24",32x"00", x"00000000",'0','0','0',  '1','1',30x"09",'0',x"1",x"00000000", x"00000000",'0','0'),
        ('0', '1',C_MEM_OP_LOADU8,  32x"25",32x"00", x"87654321",'1','0','0',  '1','1',30x"09",'0',x"2",x"00000000", x"00000021",'1','0'),
        ('0', '1',C_MEM_OP_LOADU8,  32x"26",32x"00", x"87654321",'1','0','0',  '1','1',30x"09",'0',x"4",x"00000000", x"00000043",'1','0'),
        ('0', '1',C_MEM_OP_LOADU8,  32x"27",32x"00", x"87654321",'1','0','0',  '1','1',30x"09",'0',x"8",x"00000000", x"00000065",'1','0'),
        ('0', '1',C_MEM_OP_LOAD8,   32x"24",32x"00", x"87654321",'1','0','0',  '1','1',30x"09",'0',x"1",x"00000000", x"00000087",'1','0'),
        ('0', '1',C_MEM_OP_LOAD8,   32x"25",32x"00", x"87654321",'1','0','0',  '1','1',30x"09",'0',x"2",x"00000000", x"00000021",'1','0'),
        ('0', '1',C_MEM_OP_LOAD8,   32x"26",32x"00", x"87654321",'1','0','0',  '1','1',30x"09",'0',x"4",x"00000000", x"00000043",'1','0'),
        ('0', '1',C_MEM_OP_LOAD8,   32x"27",32x"00", x"87654321",'1','0','0',  '1','1',30x"09",'0',x"8",x"00000000", x"00000065",'1','0'),
        ('0', '1',C_MEM_OP_STORE8,  32x"28",32x"12", x"87654321",'1','0','0',  '1','1',30x"0a",'1',x"1",x"00000012", x"ffffff87",'1','0'),
        ('0', '1',C_MEM_OP_STORE8,  32x"29",32x"34", x"00000000",'1','0','0',  '1','1',30x"0a",'1',x"2",x"00003400", x"00000000",'0','0'),
        ('0', '1',C_MEM_OP_STORE8,  32x"2a",32x"56", x"00000000",'1','0','0',  '1','1',30x"0a",'1',x"4",x"00560000", x"00000000",'0','0'),
        ('0', '1',C_MEM_OP_STORE8,  32x"2b",32x"78", x"00000000",'1','0','0',  '1','1',30x"0a",'1',x"8",x"78000000", x"00000000",'0','0'),
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000000",'1','0','0',  '1','0',30x"00",'0',x"0",x"00000000", x"00000000",'0','0'),

        -- Pipelined read burst from the memory, with two-cycle stall.
        ('0', '1',C_MEM_OP_LOAD32,  32x"24",32x"00", x"00000000",'0','0','0',  '1','1',30x"09",'0',x"f",x"00000000", x"00000000",'0','0'), -- 26
        ('0', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000012",'1','1','0',  '1','1',30x"0a",'0',x"f",x"00000000", x"00000012",'1','1'),
        ('0', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000000",'0','1','0',  '1','1',30x"0a",'0',x"f",x"00000000", x"00000012",'1','1'),
        ('0', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000000",'0','0','0',  '1','1',30x"0a",'0',x"f",x"00000000", x"00000012",'1','0'),
        ('0', '1',C_MEM_OP_LOAD32,  32x"2c",32x"00", x"00000034",'1','0','0',  '1','1',30x"0b",'0',x"f",x"00000000", x"00000034",'1','0'),
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000056",'1','0','0',  '1','0',30x"00",'0',x"0",x"00000000", x"00000056",'1','0'),

        -- Pipelined read burst from the memory, with delayed ack and three-cycle stall.
        ('0', '1',C_MEM_OP_LOAD32,  32x"24",32x"00", x"00000000",'0','0','0',  '1','1',30x"09",'0',x"f",x"00000000", x"00000000",'0','0'), -- 32
        ('1', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000000",'0','0','0',  '1','0',30x"0a",'0',x"f",x"00000000", x"00000000",'0','0'),
        ('1', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000012",'1','0','0',  '1','0',30x"0a",'0',x"f",x"00000000", x"00000012",'1','0'),
        ('1', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000000",'0','0','0',  '0','0',30x"0a",'0',x"f",x"00000000", x"00000012",'1','0'),
        ('0', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000000",'0','0','0',  '1','1',30x"0a",'0',x"f",x"00000000", x"00000012",'1','0'),
        ('0', '1',C_MEM_OP_LOAD32,  32x"2c",32x"00", x"00000034",'1','0','0',  '1','1',30x"0b",'0',x"f",x"00000000", x"00000034",'1','0'),
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000056",'1','0','0',  '1','0',30x"00",'0',x"0",x"00000000", x"00000056",'1','0'),

        -- Pipelined read burst with one long (multi cycle) memory request.
        ('0', '1',C_MEM_OP_LOAD32,  32x"24",32x"00", x"00000000",'0','0','0',  '1','1',30x"09",'0',x"f",x"00000000", x"00000000",'0','0'),
        ('0', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000000",'0','0','0',  '1','0',30x"0a",'0',x"f",x"00000000", x"00000000",'0','1'),
        ('0', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000000",'0','0','0',  '1','0',30x"0a",'0',x"f",x"00000000", x"00000000",'0','1'),
        ('0', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000000",'0','0','0',  '1','0',30x"0a",'0',x"f",x"00000000", x"00000000",'0','1'),
        ('0', '1',C_MEM_OP_LOAD32,  32x"28",32x"00", x"00000012",'1','0','0',  '1','1',30x"0a",'0',x"f",x"00000000", x"00000012",'1','0'),
        ('0', '1',C_MEM_OP_LOAD32,  32x"2c",32x"00", x"00000034",'1','0','0',  '1','1',30x"0b",'0',x"f",x"00000000", x"00000034",'1','0'),
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000056",'1','0','0',  '1','0',30x"00",'0',x"0",x"00000000", x"00000056",'1','0'),

        -- Pipelined write burst to the memory, with one cycle stall.

        -- Pipelined write burst to the memory, with one cycle stall.

        -- Tail (inactive).
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000000",'0','0','0',  '0','0',30x"00",'0',x"0",x"00000000", x"00000000",'0','0'),
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000000",'0','0','0',  '0','0',30x"00",'0',x"0",x"00000000", x"00000000",'0','0'),
        ('0', '0',C_MEM_OP_NONE,    32x"00",32x"00", x"00000000",'0','0','0',  '0','0',30x"00",'0',x"0",x"00000000", x"00000000",'0','0')
      );
  begin
    -- Clear all input signals.
    s_stall <= '0';
    s_mem_enable <= '0';
    s_mem_op <= (others => '0');
    s_mem_adr <= (others => '0');
    s_mem_dat <= (others => '0');
    s_cache_dat <= (others => '0');
    s_cache_ack <= '0';
    s_cache_stall <= '0';
    s_cache_err <= '0';

    -- Start by resetting the DUT (to have a defined state).
    s_rst <= '1';
    s_clk <= '0';
    wait for 1 ns;
    s_clk <= '1';
    wait for 1 ns;
    s_clk <= '0';
    s_rst <= '0';
    wait for 1 ns;

    -- Test all the patterns in the pattern array.
    for i in patterns'range loop
      -- Positivie clock flank (tick registers).
      s_clk <= '1';
      wait until s_clk = '1';

      -- Set the inputs for this cycle.
      s_stall <= patterns(i).stall;
      s_mem_enable <= patterns(i).mem_enable;
      s_mem_op <= patterns(i).mem_op;
      s_mem_adr <= patterns(i).mem_adr;
      s_mem_dat <= patterns(i).mem_dat;
      s_cache_dat <= patterns(i).cache_dat;
      s_cache_ack <= patterns(i).cache_ack;
      s_cache_stall <= patterns(i).cache_stall;
      s_cache_err <= patterns(i).cache_err;

      -- Wait for the results.
      wait for 1 ns;

      -- Check the outputs.
      assert s_cache_cyc = patterns(i).cache_cyc
        report "Bad result (" & integer'image(i) & "):" & lf &
               "  cache_cyc = " & to_string(s_cache_cyc) & lf &
               "  expected " & to_string(patterns(i).cache_cyc)
            severity error;
      assert s_cache_stb = patterns(i).cache_stb
        report "Bad result (" & integer'image(i) & "):" & lf &
               "  cache_stb = " & to_string(s_cache_stb) & lf &
               "  expected " & to_string(patterns(i).cache_stb)
            severity error;
      assert s_cache_adr = patterns(i).cache_adr or (s_cache_stb and s_cache_cyc) = '0'
        report "Bad result (" & integer'image(i) & "):" & lf &
               "  cache_adr = " & to_string(s_cache_adr) & lf &
               "  expected " & to_string(patterns(i).cache_adr)
            severity error;
      assert s_cache_we = patterns(i).cache_we or (s_cache_stb and s_cache_cyc) = '0'
        report "Bad result (" & integer'image(i) & "):" & lf &
               "  cache_we = " & to_string(s_cache_we) & lf &
               "  expected " & to_string(patterns(i).cache_we)
            severity error;
      assert s_cache_sel = patterns(i).cache_sel or (s_cache_stb and s_cache_cyc) = '0'
        report "Bad result (" & integer'image(i) & "):" & lf &
               "  cache_sel = " & to_string(s_cache_sel) & lf &
               "  expected " & to_string(patterns(i).cache_sel)
            severity error;
      assert s_cache_dat_out = patterns(i).cache_dat_out or (s_cache_stb and s_cache_cyc) = '0'
        report "Bad result (" & integer'image(i) & "):" & lf &
               "  cache_dat_out = " & to_string(s_cache_dat_out) & lf &
               "  expected " & to_string(patterns(i).cache_dat_out)
            severity error;
      assert s_result = patterns(i).result or s_result_ready = '0'
        report "Bad result (" & integer'image(i) & "):" & lf &
               "  result = " & to_string(s_result) & lf &
               "  expected " & to_string(patterns(i).result)
            severity error;
      assert s_result_ready = patterns(i).result_ready
        report "Bad result (" & integer'image(i) & "):" & lf &
               "  result_ready = " & to_string(s_result_ready) & lf &
               "  expected " & to_string(patterns(i).result_ready)
            severity error;
      assert s_stall_out = patterns(i).stall_out
        report "Bad result (" & integer'image(i) & "):" & lf &
               "  stall_out = " & to_string(s_stall_out) & lf &
               "  expected " & to_string(patterns(i).stall_out)
            severity error;

      -- Tick the clock.
      s_clk <= '0';
      wait for 1 ns;
    end loop;
    assert false report "End of test" severity note;
    --  Wait forever; this will finish the simulation.
    wait;
  end process;
end behavioral;

