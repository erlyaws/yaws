include @top_srcdir@/include.mk

MODULES = app_test.erl

EXTRA_DIST = $(MODULES)	bad_redirect1.conf bad_redirect2.conf bad_redirect3.conf bad_redirect4.conf www

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

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
