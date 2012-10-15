include ../support/include.mk

.PHONY: all test conf debug clean

#
all:	conf setup app_test.beam
	@echo "all ok"


# invoke as
# TEST=test3 make test
# or just make test to run all

ULIMIT = 768

test:	all start
	ul=`ulimit -n` ; \
	val=`expr $$ul '<' $(ULIMIT)` ; \
	if [ $$val = 1 ] ; then \
	    echo trying to raise "ulimit -n" for the test... ; \
	    set -e ; \
	    ulimit -n $(ULIMIT) ; \
	fi ; \
	$(ERL) -sname tftest -noinput $(PA) -s tftest
	$(MAKE) stop

conf:	mimetypes

debug:
	$(ERL) $(PA)

clean:	tclean
	-rm -rf logs yaws.conf
