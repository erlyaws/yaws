include @top_srcdir@/test/support/include.mk

EXTRA_DIST = wgettest runtest
ERL_FLAGS  = $(ERL_GENERIC_FLAGS)
YAWS_CONF=yaws.conf

DATAFILE_BASE = small_datafile.dat
DATAFILE      = ./docroot-test/$(DATAFILE_BASE)
TMP_DATAFILE  = ./docroot-test/tmpdatafile

CHECK_WITH_CURL =								\
	set -e;									\
	rm -f $(TMP_DATAFILE);							\
	curl -s -m 5 -o $(TMP_DATAFILE) http://localhost:8002/$(DATAFILE_BASE); \
	cmp -s $(DATAFILE) $(TMP_DATAFILE)

all-local:

clean-local: clean-test

check-local: log start wait-started t1 t2 t3 t4 t5 t6 t7 stop json

log:
	@echo
	@echo " ==== TESTS ==== "
	@echo

prepare-test::
	$(AM_V_at)cp $(YAWS_CONF) $(YAWS_CONF).orig
	$(AM_V_at)dd if=/dev/zero of=$(DATAFILE)                bs=10   count=1    >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/131072.dat  bs=1024 count=128  >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/1048576.dat bs=1024 count=1024 >/dev/null 2>&1
#	$(AM_V_at)dd if=/dev/zero of=./docroot-test/8388608.dat bs=1024 count=8192 >/dev/null 2>&1

clean-test::
	$(AM_V_at)[ ! -f $(YAWS_CONF).orig ] || mv $(YAWS_CONF).orig $(YAWS_CONF)
	$(AM_V_at)rm -fr "./localhost:8001"

newconf:
	$(AM_V_at)cp $(YAWS_CONF).orig $(YAWS_CONF)

t1:
	@echo "wget test"
	$(AM_V_at)rm -fr "./localhost:8001"
	$(AM_V_at)$(SHELL) @srcdir@/wgettest
	$(AM_V_at)rm -fr "./localhost:8001"

t2:
	@echo "sendfile test"
	$(AM_V_at)$(SHELL) @srcdir@/runtest 131072 1048576 #8388608

t3:
	$(AM_V_at)if [ -z $(CURL) ]; then				\
	  echo "SKIPPED - curl is required to run these tests";		\
	else								\
	  echo "check-with-curl (max_num_cached_files = 0)";		\
	  cp $(YAWS_CONF).orig $(YAWS_CONF);				\
	  echo 'max_num_cached_files = 0' >> $(YAWS_CONF);		\
	  $(top_builddir)/test/bin/yaws --id testid --hup >/dev/null;	\
	  $(CHECK_WITH_CURL);						\
	fi

t4:
	$(AM_V_at)if [ -z $(CURL) ]; then				\
	  echo "SKIPPED - curl is required to run these tests";		\
	else								\
	  echo "check-with-curl (max_num_cached_bytes = 0)";		\
	  cp $(YAWS_CONF).orig $(YAWS_CONF);				\
	  echo 'max_num_cached_bytes = 0' >> $(YAWS_CONF);		\
	  $(top_builddir)/test/bin/yaws --id testid --hup >/dev/null;	\
	  $(CHECK_WITH_CURL);						\
	fi

t5:
	$(AM_V_at)if [ -z $(CURL) ]; then				\
	  echo "SKIPPED - curl is required to run these tests";		\
	else								\
	  echo "check-with-curl (max_size_cached_file = 0)";		\
	  cp $(YAWS_CONF).orig $(YAWS_CONF);				\
	  echo 'max_size_cached_file = 0' >> $(YAWS_CONF);		\
	  $(top_builddir)/test/bin/yaws --id testid --hup >/dev/null;	\
	  $(CHECK_WITH_CURL);						\
	fi

t6:
	$(AM_V_at)if [ -z $(CURL) ]; then						\
	  echo "SKIPPED - curl is required to run these tests";				\
	else										\
	  echo "check-with-curl (max_num_cached_bytes = 5 / max_size_cached_file = 50)";\
	  cp $(YAWS_CONF).orig $(YAWS_CONF);						\
	  echo 'max_num_cached_bytes = 5'  >> $(YAWS_CONF);				\
	  echo 'max_size_cached_file = 50' >> $(YAWS_CONF);				\
	  $(top_builddir)/test/bin/yaws --id testid --hup >/dev/null;			\
	  $(CHECK_WITH_CURL);								\
	fi

# Verify redirection does not create URIs with multiple adjacent slashes Test
# submitted by James Lee
t7: newconf hup
	$(AM_V_at)if [ -z $(CURL) ]; then				\
	  echo "SKIPPED - curl is required to run these tests";		\
	else								\
	  echo "redirect test";						\
	  loc=`curl -s -m 5 -i http://localhost:8003/foo   |		\
	    perl -ne 'print "$$1\n" if (m#Location:\s+http://(.+)#)' | 	\
	    grep //`;							\
	  [ "${loc}" = "" ] && exit 0 || exit 1;			\
	fi

json:
	@echo "json test"
	$(AM_V_at)$(ERL) -noinput $(ERL_FLAGS) -eval			\
	    'case json2:test() of ok -> halt(0); _ -> halt(1) end'


# Local Variables:
#    tab-width: 8
# End:
