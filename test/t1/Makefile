include ../support/include.mk

all:	conf setup
	echo "all ok"

## to run test, do
# make all start test

test:	
	$(MAKE) t1 t2
	$(MAKE) stop

t1:
	$(SHELL) ./wgettest

t2:
	sizes='131072 1048576 8388608' ; \
	for sz in $$sizes; do \
            dd if=/dev/zero \
            of=../../www/$$sz.dat count=1 bs=$$sz >/dev/null 2>&1 ; \
        done ; \
	$(SHELL) ./runtest $$sizes


conf:	stdconf

clean:	tclean
	-rm -rf localhost:8000




