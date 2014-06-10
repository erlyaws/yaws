include @top_srcdir@/include.mk

MODULES = test.erl tftest.erl

EXTRA_DIST = $(MODULES)

EBIN_FILES=$(MODULES:%.erl=%.beam)

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS)

include @top_srcdir@/erlang_deps.mk

all-local: $(EBIN_FILES)

clean-local:
	$(AM_V_at)rm -f $(EBIN_FILES)

%.beam: %.erl
	$(AM_V_at)$(ERLC) $(ERLC_FLAGS) -M -MF $(@:%.beam=$(DEPDIR)/%.Pbeam) -MT $@ $<
	$(AM_V_ERLC)$(ERLC) $(ERLC_FLAGS) -o . $<


distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
