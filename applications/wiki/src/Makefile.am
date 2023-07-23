include @top_srcdir@/include.mk

MODULES = utils.erl			\
	  wiki_diff.erl			\
	  wiki.erl			\
	  wiki_format_txt.erl		\
	  wiki_plugin_backlinks.erl	\
	  wiki_plugin_dummy.erl		\
	  wiki_plugin_menu.erl		\
	  wiki_split.erl		\
	  wiki_templates.erl		\
	  wiki_to_html.erl		\
	  wiki_utils.erl		\
	  wiki_yaws.erl


EXTRA_DIST = $(MODULES)

EBIN_DIR=@builddir@/../ebin
EBIN_FILES=$(MODULES:%.erl=$(EBIN_DIR)/%.beam)

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS)

include @top_srcdir@/erlang_deps.mk

all-local: $(EBIN_FILES)

clean-local:
	$(AM_V_at)rm -f $(EBIN_FILES)

$(EBIN_DIR)/%.beam: %.erl
	$(AM_V_at)$(ERLC) $(ERLC_FLAGS) -M $(PHONY_DEPS) -MF $(@:$(EBIN_DIR)/%.beam=$(DEPDIR)/%.Pbeam) -MT $@ $<
	$(AM_V_ERLC)$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<


distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
