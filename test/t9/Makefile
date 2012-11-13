include ../support/include.mk

.PHONY: all test conf debug clean

#
all: conf setup app_test.beam
	@echo "all ok"


# invoke as
# TEST=test3 make test
# or just make test to run all

test: all start
	$(ERL) -noinput $(PA) -s tftest
	$(MAKE) stop

conf: redirectconf

debug:
	$(ERL) $(PA)

clean: tclean
	-rm -rf logs yaws.conf
