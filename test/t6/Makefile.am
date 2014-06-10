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

$(NO_DOCROOT_TESTS): $(top_builddir)/test/bin/yaws
	$(AM_V_at)$(INSTALL) -d logs
	$(AM_V_at)$(top_builddir)/test/bin/yaws -sname test --daemon --id testid --conf $(YAWS_CONF)
	$(AM_V_at)$(top_builddir)/test/bin/yaws --id testid --wait-started=$(WAIT_TIME)
	@echo $@
	$(AM_V_at)$(top_builddir)/test/bin/yaws --id testid --stop >/dev/null
	$(AM_V_at)$(top_builddir)/test/bin/yaws --id testid --wait-stopped=$(WAIT_TIME)

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
