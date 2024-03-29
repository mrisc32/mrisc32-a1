####################################################################################################
# Copyright (c) 2018 Marcus Geelnard
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

WORKDIR = out
SW_SRC_DIR = ../../sw

GHDL = ghdl
GHDLWARNINGS = \
    -Wbinding \
    -Wreserved \
    -Wlibrary \
    -Wvital-generic \
    -Wbody \
    -Wspecs \
    -Wunused \
    -Werror
GHDLFLAGS = --std=08 -g --work=work --workdir=$(WORKDIR) $(GHDLWARNINGS)

.PHONY: all clean

default: all

SRCS =
TESTS =
EXTRA =

include rtl/Makefile
include test/Makefile
include benchmark/Makefile
include test/programs/Makefile

all: $(TESTS) $(EXTRA)

$(TESTS): $(WORKDIR)/work-obj93.cf $(SRCS)
	$(GHDL) -m $(GHDLFLAGS) $@

clean:
	$(GHDL) --clean
	rm -rf *.o *_tb *.vcd *.ghw $(WORKDIR)/*.o $(WORKDIR)/*.cf $(WORKDIR)/*_tb $(WORKDIR)/*.vcd $(WORKDIR)/*.ghw

$(WORKDIR)/work-obj93.cf: $(SRCS)
	$(GHDL) -i $(GHDLFLAGS) $(SRCS)

