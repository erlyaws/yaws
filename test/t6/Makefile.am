NO_DOCROOT_TESTS = no_docroot_revproxy	\
		   no_docroot_fwdproxy 	\
		   no_docroot_redirect 	\
		   no_docroot_appmod

include @top_srcdir@/test/support/include.mk

all-local:

clean-local: clean-test

check-local: log $(NO_DOCROOT_TESTS)

log:
	@echo
	@echo " ==== NO DOCROOT TESTS ==== "
	@echo

gen_rule = $1: YAWS_CONF=$1.conf

$(foreach r,$(NO_DOCROOT_TESTS), $(eval $(call gen_rule,$r)))

$(NO_DOCROOT_TESTS): quiet-stop prepare-test
	$(AM_V_at)$(INSTALL) -d logs
	$(AM_V_at)$(start_yaws)
	$(AM_V_at)$(wait_yaws_started)
	@echo $@
	$(AM_V_at)$(stop_yaws)
	$(AM_V_at)$(wait_yaws_stopped)

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
