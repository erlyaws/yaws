include @top_srcdir@/include.mk

MODULES = app_test.erl			\
	  appmod1.erl			\
	  appmod2.erl			\
	  appmod3.erl			\
	  cache_appmod_test.erl		\
	  dispatchmod_tester.erl	\
	  flushtest.erl			\
	  forwarded_for_test.erl	\
	  jsontest.erl			\
	  phptest.erl			\
	  posttest.erl			\
	  reentranttest.erl		\
	  rewritetest.erl		\
	  shapertest.erl		\
	  streamtest.erl		\
	  throwtest.erl			\
	  wrap_log.erl

EXTRA_DIST = $(MODULES) www

EBIN_FILES=$(MODULES:%.erl=%.beam)

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS) -pa $(top_builddir)/test/ibrowse
ERL_FLAGS  = $(ERL_GENERIC_FLAGS)

include @top_srcdir@/erlang_deps.mk
include @top_srcdir@/test/support/include.mk

YAWS_CONF=yaws.conf

all-local:

clean-local: clean-test
	$(AM_V_at)rm -f $(EBIN_FILES)

check-local: $(EBIN_FILES) app-test

prepare-test::
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/1000.txt  bs=1024 count=1000  >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/2000.txt  bs=1024 count=2000  >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/3000.txt  bs=1024 count=3000  >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/10000.txt bs=1024 count=10000 >/dev/null 2>&1

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
