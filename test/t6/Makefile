include ../support/include.mk

NO_DOCROOT_TESTS =	no_docroot_revproxy \
			no_docroot_fwdproxy \
			no_docroot_redirect \
			no_docroot_appmod

.PHONY: all test conf $(NO_DOCROOT_TESTS)

all:	setup
	@echo "all ok"

## to run test, do
# make all test

test: all
	set -e ; \
	for t in $(NO_DOCROOT_TESTS); do \
	    $(MAKE) $$t ; \
	done

$(NO_DOCROOT_TESTS):
	@rm -f yaws.conf
	cat ../conf/$@.conf | \
	$(SUBST) %YTOP% $(YTOP) > yaws.conf
	set -e ; $(MAKE) start ; $(MAKE) stop

clean:	tclean
	-rm -rf localhost:8000 yaws.conf

# needed because target is called by top-level test/Makefile
conf:
