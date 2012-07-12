include ../support/include.mk

.PHONY: all test conf newconf t1 json

all:	conf setup
	@echo "all ok"

## to run test, do
# make all test

test: all
	set -e ; \
	for t in t1; do \
	    $(MAKE) $$t ; \
	done

newconf:
	@rm -f yaws.conf
	$(MAKE) conf

t1: newconf start
	$(SHELL) ./davtest
	$(MAKE) stop

conf:	davconf

clean:	tclean
	-rm -rf $(TMP_DATAFILE) yaws.conf
