include ../support/include.mk

all:	conf setup
	@echo "all ok"

## to run test, do
# make all test

test:	
	$(MAKE) t1 t2 t3 t4 t5 t6

t1: start
	$(SHELL) ./wgettest
	$(MAKE) stop

t2: start
	sizes='131072 1048576 8388608' ; \
	for sz in $$sizes; do \
            dd if=/dev/zero \
            of=../../www/$$sz.dat count=1 bs=$$sz >/dev/null 2>&1 ; \
	done ; \
	$(SHELL) ./runtest $$sizes
	$(MAKE) stop

DATAFILE_BASE = small_datafile.dat
DATAFILE = ../../www/$(DATAFILE_BASE)
TMP_DATAFILE = ./tmpdatafile

CHECK_WITH_CURL = \
	set -e ; \
	$(MAKE) start ; \
	trap 'rm -f $(TMP_DATAFILE) ; $(MAKE) stop ; sleep 3' HUP INT EXIT ; \
	curl -s -m 5 -o $(TMP_DATAFILE) http://localhost:8000/$(DATAFILE_BASE) ; \
	cmp -s $(DATAFILE) $(TMP_DATAFILE)

small_datafile:
	dd if=/dev/zero of=$(DATAFILE) count=1 bs=10 >/dev/null 2>&1

t3: small_datafile
	@rm -f yaws.conf
	$(MAKE) conf
	echo 'max_num_cached_files = 0' >> yaws.conf
	$(CHECK_WITH_CURL)

t4: small_datafile
	@rm -f yaws.conf
	$(MAKE) conf
	echo 'max_num_cached_bytes = 0' >> yaws.conf
	$(CHECK_WITH_CURL)

t5: small_datafile
	@rm -f yaws.conf
	$(MAKE) conf
	echo 'max_size_cached_file = 0' >> yaws.conf
	$(CHECK_WITH_CURL)

t6: small_datafile
	@rm -f yaws.conf
	$(MAKE) conf
	echo 'max_num_cached_bytes = 5' >> yaws.conf
	echo 'max_size_cached_file = 50' >> yaws.conf
	$(CHECK_WITH_CURL)

conf:	stdconf

clean:	tclean
	-rm -rf localhost:8000 $(TMP_DATAFILE) yaws.conf
