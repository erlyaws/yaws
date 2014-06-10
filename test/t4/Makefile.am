include @top_srcdir@/include.mk

MODULES = app_test.erl		\
	  rewritetest.erl	\
	  posttest.erl		\
	  streamtest.erl	\
	  nolengthtest.erl	\
	  intercept1.erl	\
	  intercept2.erl	\
	  intercept3.erl


EXTRA_DIST = $(MODULES)

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
	$(AM_V_at)mkdir docroot-test1
	$(AM_V_at)mkdir docroot-test2
	$(AM_V_at)echo "Hello World!"   > docroot-test/index.html
	$(AM_V_at)echo "Hello World 1!" > docroot-test1/hello.txt
	$(AM_V_at)echo "Hello World 2!" > docroot-test2/hello.txt
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/1000.txt     bs=1024 count=1000  >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/2000.txt     bs=1024 count=2000  >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/3000.txt     bs=1024 count=3000  >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/10000.txt    bs=1024 count=10000 >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test2/8388608.txt bs=1024 count=8192  >/dev/null 2>&1

clean-test::
	$(AM_V_at)rm -fr docroot-test1 docroot-test2
distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
