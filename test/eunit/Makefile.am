include @top_srcdir@/include.mk

MODULES = multipart_post_parsing.erl	\
	  yaws_session_server_test.erl	\
	  ehtml_test.erl		\
	  cookies.erl			\
	  headers.erl			\
	  ipmask.erl			\
	  gconf.erl			\
	  sconf.erl			\
	  yaws_rpc_test.erl		\
	  proc_cleanup.erl		\
	  subconfig.erl			\
	  copy_error_log.erl		\
	  dynopts.erl			\
	  yaws_ls_tests.erl		\
	  json2_decode.erl

EXTRA_DIST = $(MODULES) cookies.dump setcookies.dump subconfig_DATA

EBIN_FILES=$(MODULES:%.erl=%.beam)

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS)
ERL_FLAGS  = $(ERL_GENERIC_FLAGS)

include @top_srcdir@/erlang_deps.mk
include @top_srcdir@/test/support/include.mk

EUNIT_TESTS = $(MODULES:.erl=)

all-local:

clean-local: clean-test
	$(AM_V_at)rm -f $(EBIN_FILES) client.log

log:
	@echo
	@echo " ==== EUNIT TESTS ==== "
	@echo

$(EUNIT_TESTS): $(EBIN_FILES) $(top_builddir)/test/src/test.beam $(top_builddir)/test/bin/yaws
	$(AM_V_at)$(quiet_stop_yaws)
	$(AM_V_at)$(wait_stopped)
	$(AM_V_at)$(ERL) $(ERL_GENERIC_FLAGS) -noshell -s test run $@

check-local: log $(EUNIT_TESTS)

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
