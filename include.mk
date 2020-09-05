AM_V_ERLC = $(am__v_ERLC_$(V))
am__v_ERLC_ = $(am__v_ERLC_$(AM_DEFAULT_VERBOSITY))
am__v_ERLC_0 = @echo "  ERLC    " $@;
am__v_ERLC_1 =

# TODO -Werror is turned off until the deprecated ssl:cipher_suites/0
# can be fixed
#WARNINGS_AS_ERRORS = -Werror
WARNINGS_AS_ERRORS =

ERLC_GENERIC_FLAGS = $(WARNINGS_AS_ERRORS) +debug_info $(DEBUG_ERLC_FLAGS)		\
		     -pa $(top_srcdir) -pa $(top_builddir) -pa $(top_builddir)/ebin	\
		     -I $(top_srcdir)/include -I $(srcdir)/../include 			\
		     -I $(top_builddir)/include -I $(builddir)/../include

# Local Variables:
#    tab-width: 8
# End:
