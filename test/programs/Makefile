# -*- mode: Makefile; tab-width: 8; indent-tabs-mode: t; -*-
####################################################################################################
# Copyright (c) 2021 Marcus Geelnard
#
# This software is provided 'as-is', without any express or implied warranty. In no event will the
# authors be held liable for any damages arising from the use of this software.
#
# Permission is granted to anyone to use this software for any purpose, including commercial
# applications, and to alter it and redistribute it freely, subject to the following restrictions:
#
#  1. The origin of this software must not be misrepresented; you must not claim that you wrote
#     the original software. If you use this software in a product, an acknowledgment in the
#     product documentation would be appreciated but is not required.
#
#  2. Altered source versions must be plainly marked as such, and must not be misrepresented as
#     being the original software.
#
#  3. This notice may not be removed or altered from any source distribution.
####################################################################################################

TEST_PROGRAMS_PATH = test/programs

EXTRA += \
    $(WORKDIR)/branches.bin \
    $(WORKDIR)/memory.bin \
    $(WORKDIR)/simple.bin

$(WORKDIR)/branches.bin: $(TEST_PROGRAMS_PATH)/branches.s
	mrisc32-elf-gcc -mno-crt0 -mno-ctor-dtor $< -o $(WORKDIR)/branches.elf
	mrisc32-elf-objcopy -O binary $(WORKDIR)/branches.elf $@

$(WORKDIR)/memory.bin: $(TEST_PROGRAMS_PATH)/memory.s
	mrisc32-elf-gcc -mno-crt0 -mno-ctor-dtor $< -o $(WORKDIR)/memory.elf
	mrisc32-elf-objcopy -O binary $(WORKDIR)/memory.elf $@

$(WORKDIR)/simple.bin: $(TEST_PROGRAMS_PATH)/simple.s
	mrisc32-elf-gcc -mno-crt0 -mno-ctor-dtor $< -o $(WORKDIR)/simple.elf
	mrisc32-elf-objcopy -O binary $(WORKDIR)/simple.elf $@

