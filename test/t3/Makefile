include ../support/include.mk

.PHONY: all test debug clean

#
all:	conf setup app_test.beam authmodtest.beam outmodtest.beam
	@echo "all ok"


# invoke as
# TEST=test3 make test
# or just make test to run all

ULIMIT = 768

test:	conf start
	ul=`ulimit -n` ; \
	val=`expr $$ul '<' $(ULIMIT)` ; \
	if [ $$val = 1 ] ; then \
	    echo trying to raise "ulimit -n" for the test... ; \
	    set -e ; \
	    ulimit -n $(ULIMIT) ; \
	fi ; \
	$(ERL) -noinput $(PA) -s tftest
	$(MAKE) stop

conf:	authconf

debug:
	$(ERL) $(PA)

clean:	tclean
	-rm -rf localhost:8000 logs yaws.conf
