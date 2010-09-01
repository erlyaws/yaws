include ../support/include.mk

.PHONY: all test debug clean

#
all:	conf setup app_test.beam streamtest.beam
	@echo "all ok"


# invoke as
# TEST=test3 make test
# or just make test to run all

ULIMIT = 768

test:	conf start
	dd if=/dev/zero of=../../www/1000.txt bs=1024 count=1000 >/dev/null 2>&1
	dd if=/dev/zero of=../../www/2000.txt bs=1024 count=2000 >/dev/null 2>&1
	dd if=/dev/zero of=../../www/3000.txt bs=1024 count=3000 >/dev/null 2>&1
	dd if=/dev/zero of=../../www/10000.txt bs=1024 count=10000 >/dev/null 2>&1
	ul=`ulimit -n` ; \
	val=`expr $$ul '<' $(ULIMIT)` ; \
	if [ $$val = 1 ] ; then \
	    echo trying to raise "ulimit -n" for the test... ; \
	    set -e ; \
	    ulimit -n $(ULIMIT) ; \
	fi ; \
	$(ERL) -noinput $(PA) -s tftest
	$(MAKE) stop

conf:	stdconf

debug:
	$(ERL) $(PA)

clean:	tclean
	-rm -rf localhost:8000 logs yaws.conf
