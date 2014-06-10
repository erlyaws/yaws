EXTRA_DIST = README include/tftest.hrl support/include.mk

if !MINGW

SUBDIRS  = src eunit t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11

include @srcdir@/support/include.mk

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS) -pa $(top_builddir)/test/ibrowse
ERL_FLAGS  = $(ERL_GENERIC_FLAGS)

all-local:

clean-local:
	$(AM_V_at)rm -rf ibrowse bin logs

test: check

else

check-local:
	@echo
	@echo "        SKIPPED - Tests cannot be executed when Yaws is cross-compiled"
	@echo

test: check

endif
# Local Variables:
#    tab-width: 8
# End:
