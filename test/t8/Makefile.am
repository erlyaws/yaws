EXTRA_DIST = davtest davtest.in

include @top_srcdir@/test/support/include.mk

YAWS_CONF=yaws.conf

all-local:

clean-local: clean-test

check-local: $(EBIN_FILES) log start wait-started run-davtest stop wait-stopped

log:
	@echo
	@echo " ==== WEBDAV TESTS ==== "
	@echo

run-davtest:
	$(AM_V_at)if [ -z $(CADAVER) ]; then					\
	  echo "SKIPPED - cadaver is required to run these tests";		\
	else									\
	  echo "davtest";							\
	  cd @srcdir@ && ./davtest ./davtest.in @abs_builddir@/logs/davtest.out;\
	  err=$$?;								\
	  if test $$err -ne 0; then						\
	    $(MAKE) quiet-stop;							\
	    exit $$err;								\
	  fi									\
	fi

prepare-test::
	$(AM_V_at)echo "test" > ./docroot-test/test

# Local Variables:
#    tab-width: 8
# End:
