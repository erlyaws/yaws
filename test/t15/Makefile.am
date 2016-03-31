include @top_srcdir@/include.mk

MODULES = app_test.erl

EXTRA_DIST = $(MODULES) www

EBIN_FILES=$(MODULES:%.erl=%.beam)

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS) -pa $(top_builddir)/test/ibrowse
ERL_FLAGS  = $(ERL_GENERIC_FLAGS)

include @top_srcdir@/erlang_deps.mk
include @top_srcdir@/test/support/include.mk

SNI_GOOD_TESTS = sni_disabled
SNI_BAD_TESTS  = sni_not_enabled

if HAVE_SSL_SNI
SNI_GOOD_TESTS += sni_enabled sni_strict sni_required_on_vhost
SNI_BAD_TESTS  += sni_without_tls
else
SNI_BAD_TESTS  += sni_not_available
endif

gen_rule = $1: YAWS_CONF=yaws_$1.conf

$(foreach r,$(SNI_GOOD_TESTS), $(eval $(call gen_rule,$r)))
$(foreach r,$(SNI_BAD_TESTS),  $(eval $(call gen_rule,$r)))

all-local:

clean-local: clean-test
	$(AM_V_at)rm -f $(EBIN_FILES)
log:
	@echo
	@echo " ==== SSL SNI TESTS ==== "
	@echo

$(SNI_GOOD_TESTS): quiet-stop prepare-test
	$(AM_V_at)$(start_yaws)
	$(AM_V_at)$(wait_yaws_started)
	$(AM_V_at)TEST=$@ $(do_test)
	$(AM_V_at)$(stop_yaws)
	$(AM_V_at)$(wait_yaws_stopped)

$(SNI_BAD_TESTS): quiet-stop prepare-test
	@echo $@
	$(AM_V_at)$(start_yaws)
	$(AM_V_at)(sleep 2 && $(get_yaws_status) && exit 1 || true)
	$(AM_V_at)$(quiet-stop)

check-local: $(EBIN_FILES) log $(SNI_GOOD_TESTS) $(SNI_BAD_TESTS)

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
