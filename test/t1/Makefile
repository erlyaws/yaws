include ../support/include.mk

all:	conf setup
	echo "all ok"

test:	all
	sizes='131072 1048576 8388608' ; \
	for sz in $$sizes; do dd if=/dev/zero of=../../www/$$sz.dat count=1 bs=$$sz >/dev/null 2>&1 ; done ; \
	$(SHELL) ./runtest $$sizes
	$(MAKE) stop

conf:	stdconf

clean:	tclean
	-rm -rf localhost:8000




