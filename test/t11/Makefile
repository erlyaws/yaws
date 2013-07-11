include ../support/include.mk

.PHONY: all test conf debug clean

#
all:	conf setup app_test.beam
	@echo "all ok"

start: all inc_v1

# invoke as
# TEST=test3 make test
# or just make test to run all

ULIMIT = 768

test:	start
	$(ERL) -noinput $(PA) -s tftest
	$(MAKE) stop

inc_v1:
	echo "-define(SRCDIR_VERSION, \"1.0\")." > src/srcdir_test.hrl

inc_v2:
	echo "-define(SRCDIR_VERSION, \"2.0\")." > src/srcdir_test.hrl
	$(YTOP)/bin/yaws --id testid --hup

conf:	srcdirconf

debug:
	$(ERL) $(PA)

clean:	tclean
	-rm -rf logs yaws.conf src/srcdir_test.hrl
