include @top_srcdir@/include.mk

# For reproducible builds. This example is not used.
# This is a little bit hacky, but no one should need these to
# use the examples.

# Don't use build environments SHELL
SHELL = /bin/sh

# Omit build environment paths
abs_builddir =
abs_srcdir =
abs_top_builddir =
abs_top_srcdir =

# Remove ac-aux/missing helper script from commands, contains absolute path.
ACLOCAL = aclocal
AUTOCONF = autoconf
AUTOHEADER = autoheader
AUTOMAKE = automake
MAKEINFO = makeinfo

# Remove ac-aux/install_sh helper script, contains absolute path.
install_sh =

MODULES = myappmod.erl

EXTRA_DIST = $(MODULES)

EBIN_DIR=@top_builddir@/examples/ebin
EBIN_FILES=$(MODULES:%.erl=$(EBIN_DIR)/%.beam)

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS)

include @top_srcdir@/erlang_deps.mk

all-local: $(EBIN_FILES)

$(EBIN_DIR)/%.beam: %.erl
	$(AM_V_at)$(ERLC) $(ERLC_FLAGS) -M $(PHONY_DEPS) -MF $(@:$(EBIN_DIR)/%.beam=$(DEPDIR)/%.Pbeam) -MT $@ $<
	$(AM_V_ERLC)$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
