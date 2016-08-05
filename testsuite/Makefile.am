if HAVE_TS_TOOLS

include @top_srcdir@/include.mk

SUITE_MODULES_BLA = $(sort $(wildcard @srcdir@/*_SUITE.erl))
SUITE_MODULES = $(SUITE_MODULES_BLA:@srcdir@/%=%)

SUITE_MODULES_DATADIR = $(wildcard @srcdir@/*_SUITE_data)

SUITE_MODULES_DATADIR_MODS_BLA = $(wildcard @srcdir@/*_SUITE_data/*.erl)
SUITE_MODULES_DATADIR_MODS = $(SUITE_MODULES_DATADIR_MODS_BLA:@srcdir@/%=%)

MODULES = $(SUITE_MODULES) $(SUITE_MODULES_DATADIR_MODS) testsuite.erl

EBIN_DIR=@top_builddir@/testsuite
EBIN_FILES=$(MODULES:%.erl=$(EBIN_DIR)/%.beam)

LOG_DIR=@builddir@/logs

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS) -DSHOW_LOG

include @top_srcdir@/erlang_deps.mk

ALL_SUITES = $(SUITE_MODULES:%.erl=%)
SUITES     = all
GRPS       = all
CASES      = all

USE_COVER    = no
USE_DEBUGGER = no
TRACES       =

EXTRA_DIST = $(MODULES) $(SUITE_MODULES_DATADIR)


all-local: $(EBIN_FILES)

clean-local:
	$(AM_V_at)rm -fr $(EBIN_FILES) $(LOG_DIR)
	$(AM_V_at)rm -fr $(wildcard @builddir@/*_SUITE_data/temp)
	$(AM_V_at)find @builddir@/*_SUITE_data -type d  -empty -delete 2>/dev/null || true

$(EBIN_DIR)/%.beam: %.erl
	$(AM_V_at)$(MKDIR_P) $(shell dirname $@)
	$(AM_V_at)$(ERLC) $(ERLC_FLAGS) -M -MF $(@:$(EBIN_DIR)/%.beam=$(DEPDIR)/%.Pbeam) -MT $@ $<
	$(AM_V_ERLC)$(ERLC) $(ERLC_FLAGS) -o $(shell dirname $@) $<

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

$(LOG_DIR):
	$(AM_V_at)mkdir -p $@

common_test: all $(LOG_DIR)
	$(AM_V_at)echo "  RUN     $@ [suites=$(SUITES) - groups=$(GRPS) - cases=$(CASES)]"
	$(AM_V_at)echo "      - use cover   : $(USE_COVER)"
	$(AM_V_at)echo "      - use debugger: $(USE_DEBUGGER)"
	$(AM_V_at)echo "      - traces      : $(TRACES)"
	$(AM_V_at)USE_COVER=$(USE_COVER) USE_DEBUGGER=$(USE_DEBUGGER) TRACES="$(TRACES)" \
		@builddir@/run_common_test "$(ALL_SUITES)" "$(SUITES)" "$(GRPS)" "$(CASES)"

check-local: common_test

list-checks:
	$(AM_V_at)echo
	$(AM_V_at)echo " CHECKS:"
	$(AM_V_at)for c in $(ALL_SUITES); do echo "    - $$c"; done
	$(AM_V_at)echo

.PHONY: common_test list-checks

else

check-local:
	$(AM_V_at)echo
	$(AM_V_at)echo "  SKIPPED - Tests cannot be executed because on or more required applications are missing"
	$(AM_V_at)echo
	$(AM_V_at)((test "x$(CT_RUN)" = "x")                            && echo "    - ct_run(1) is missing")                   || true
	$(AM_V_at)((test "$(ERLANG_LIB_DIR_common_test)" = "not found") && echo "    - \"common_test\" application is missing") || true
	$(AM_V_at)((test "$(ERLANG_LIB_DIR_eunit)" = "not found")       && echo "    - \"eunit\" application is missing")       || true
	$(AM_V_at)((test "$(ERLANG_LIB_DIR_inets)" = "not found")       && echo "    - \"inets\" application is missing")       || true
	$(AM_V_at)((test "$(ERLANG_LIB_DIR_tools)" = "not found")       && echo "    - \"tools\" application is missing")       || true
	$(AM_V_at)echo

endif
