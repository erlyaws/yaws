include @top_srcdir@/include.mk

MODULES = app_test.erl

EXTRA_DIST = $(MODULES) www

EBIN_FILES=$(MODULES:%.erl=%.beam)

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS) -pa $(top_builddir)/test/ibrowse
ERL_FLAGS  = $(ERL_GENERIC_FLAGS)

include @top_srcdir@/erlang_deps.mk
include @top_srcdir@/test/support/include.mk

HAVE_SSL_SNI := $(shell $(ERL) +B -noshell -pa $(top_builddir)/ebin -eval 'io:format("~p~n", [yaws_dynopts:have_ssl_sni()])' -s init stop)

SNI_GOOD_TESTS  = sni_disabled
SNI_GOOD_TESTS += $(if $(filter $(HAVE_SSL_SNI),true), sni_enabled sni_strict sni_required_on_vhost)
SNI_BAD_TESTS   = sni_not_enabled
SNI_BAD_TESTS  += $(if $(filter $(HAVE_SSL_SNI),true),sni_without_tls,sni_not_available)

gen_rule = $1: YAWS_CONF=yaws_$1.conf

$(foreach r,$(SNI_GOOD_TESTS), $(eval $(call gen_rule,$r)))
$(foreach r,$(SNI_BAD_TESTS),  $(eval $(call gen_rule,$r)))

all-local:

clean-local: clean-test
	$(AM_V_at)rm -f $(EBIN_FILES)
log:
	@echo
	@echo " ==== SSL SNI TESTS (SNI support: $(HAVE_SSL_SNI)) ==== "
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
