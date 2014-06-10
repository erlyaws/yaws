include @top_srcdir@/include.mk

MODULES = app_test.erl		\
	  emptytest.erl 	\
	  streamtest.erl 	\
	  smalltest.erl 	\
	  bigtest.erl

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
	$(AM_V_at)touch ./docroot-test/0.txt
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/1000.txt  bs=1024 count=1000  >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/3000.txt  bs=1024 count=3000  >/dev/null 2>&1
	$(AM_V_at)dd if=/dev/zero of=./docroot-test/10000.txt bs=1024 count=10000 >/dev/null 2>&1
	$(AM_V_at)gzip -c ./docroot-test/10000.txt > ./docroot-test/10000.txt.gz
	$(AM_V_at)cp ./docroot-test/10000.txt.gz ./docroot-test/10000.txt.old.gz
	$(AM_V_at)sleep 1
	$(AM_V_at)gunzip -c ./docroot-test/10000.txt.old.gz > ./docroot-test/10000.txt.old

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
