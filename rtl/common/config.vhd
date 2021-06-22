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

----------------------------------------------------------------------------------------------------
-- This file contains the configuration options for MRISC32-A1.
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package config is
  --------------------------------------------------------------------------------------------------
  -- CPU word size.
  -- NOTE: The word size must currently be 32 bits, so this must not be changed.
  --------------------------------------------------------------------------------------------------
  constant C_LOG2_WORD_SIZE : integer := 5;
  constant C_WORD_SIZE : integer := 2**C_LOG2_WORD_SIZE;

  --------------------------------------------------------------------------------------------------
  -- Number of registers.
  -- NOTE: The the number of register must currently be 32, so this must not be changed.
  --------------------------------------------------------------------------------------------------
  constant C_LOG2_NUM_REGS : integer := 5;
  constant C_NUM_REGS : integer := 2**C_LOG2_NUM_REGS;

  --------------------------------------------------------------------------------------------------
  -- Number of vector elements in each vector register.
  -- NOTE: The the number of vector elements should be at least 16.
  -- TODO(m): Make this part of T_CORE_CONFIG instead.
  --------------------------------------------------------------------------------------------------
  constant C_LOG2_VEC_REG_ELEMENTS : integer := 4;
  constant C_VEC_REG_ELEMENTS : integer := 2**C_LOG2_VEC_REG_ELEMENTS;

  ------------------------------------------------------------------------------------------------
  -- Hardware allocated registers.
  -- NOTE: Changing these values should be possible, but it has not been tested so it may be
  -- broken.
  ------------------------------------------------------------------------------------------------
  constant C_Z_REG  : integer := 0;   -- Z  = R0
  constant C_LR_REG : integer := 30;  -- LR = R30
  constant C_VL_REG : integer := 31;  -- VL = R31

  -- For some instructions R31 is interpreted as PC rather than VL.
  constant C_PC_REG : integer := 31;


  --------------------------------------------------------------------------------------------------
  -- Per-core configuration parameters (pass these when instantiating the core entity).
  --------------------------------------------------------------------------------------------------

  type T_CORE_CONFIG is record
    -- The start PC after reset.
    RESET_PC : std_logic_vector(C_WORD_SIZE-1 downto 0);

    -- Support vector operations (including the register file, V0-V31).
    HAS_VEC : boolean;

    -- Support packed operations (i.e. .B, .H versions of instructions).
    HAS_PO : boolean;

    -- Include hardware multiply (integer only).
    HAS_MUL : boolean;

    -- Include hardware division (integer and floating point).
    HAS_DIV : boolean;

    -- Support saturating and halving arithmetic operations.
    HAS_SA : boolean;

    -- Include an FPU.
    -- NOTE: For full floating point support, HAS_DIV must also be true.
    HAS_FP : boolean;

    -- Support the FSQRT instruction.
    -- NOTE: This has not yet been implemented, so this flag should always be set to false.
    HAS_SQRT : boolean;
  end record T_CORE_CONFIG;


  -- Full configuration.
  constant C_CORE_CONFIG_FULL : T_CORE_CONFIG := (
    RESET_PC => X"00000200",
    HAS_VEC => true,
    HAS_PO => true,
    HAS_MUL => true,
    HAS_DIV => true,
    HAS_SA => true,
    HAS_FP => true,
    HAS_SQRT => false
  );

  -- Minimal configuration.
  constant C_CORE_CONFIG_MINIMAL : T_CORE_CONFIG := (
    RESET_PC => X"00000200",
    HAS_VEC => false,
    HAS_PO => false,
    HAS_MUL => false,
    HAS_DIV => false,
    HAS_SA => false,
    HAS_FP => false,
    HAS_SQRT => false
  );

  -- Default configuration: Full.
  constant C_CORE_CONFIG_DEFAULT : T_CORE_CONFIG := C_CORE_CONFIG_FULL;
end package;
