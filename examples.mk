# Makefile fragment for reproducible builds used for example code.
# This is a little bit hacky, but no one should need these variables
# to use the examples.

# Don't use build environment SHELL
SHELL = /bin/sh

# Omit build environment paths
abs_builddir =
abs_srcdir =
abs_top_builddir =
abs_top_srcdir =

# Remove ac-aux/missing helper script from commands, as they contain
# absolute paths.
ACLOCAL = aclocal
AUTOCONF = autoconf
AUTOHEADER = autoheader
AUTOMAKE = automake
MAKEINFO = makeinfo

# Remove ac-aux/install_sh helper script, contains absolute path.
install_sh =
