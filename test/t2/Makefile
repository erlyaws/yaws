include ../support/include.mk

all:	conf setup app_test.beam
	echo "all ok"

test:	start
	$(ERL) -noinput $(PA) -s tftest
	$(MAKE) stop
debug:
	$(ERL) $(PA) -s tftest start nohalt

conf:	stdconf

clean:	tclean
	-rm -rf localhost:8000




