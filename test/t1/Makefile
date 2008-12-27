include ../support/include.mk

all:	conf setup
	echo "all ok"

test:	all
	sh ./runtest
	$(MAKE) stop

conf:	stdconf

clean:	tclean
	-rm -rf localhost:8000




