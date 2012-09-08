
SUBDIRS	=	c_src src man www/shoppingcart www/code doc scripts \
                examples/src
include ./include.mk
include ./vsn.mk

PKGCONFIG_FILES = yaws.pc

all debug clean:
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; \
		then ( cd $$d && $(MAKE) $@ ) || exit 1 ; \
	    fi ; \
	  done
	rm -rf yaws-${YAWS_VSN}.script yaws-${YAWS_VSN}.boot
	rm -rf yaws-${YAWS_VSN}.rel yaws-${YAWS_VSN}.tar.gz

cleantests:
	cd test && $(MAKE) clean

clean: cleantests

install:	all
	set -e ; \
	for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; \
		then ( cd $$d && $(MAKE) $@ ) || exit 1 ; \
	    fi ; \
	done
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/pkgconfig
	$(INSTALL) -m 644 $(PKGCONFIG_FILES) $(DESTDIR)$(LIBDIR)/pkgconfig
	@echo "-------------------------------"
	@echo
	@echo "** etc files went into        ${ETCDIR}"
	@echo "** executables went into      ${prefix}/bin"
	@echo "** library files went into    ${LIBDIR}/yaws"
	@echo "** var files went into        ${VARDIR}"
	@echo "** default docroot went into  ${VARDIR}/yaws/www"
	@echo
	@echo "--------------------------------"


docs:
	( cd doc && $(MAKE) docs )

conf_clean:
	-rm include.mk config.cache config.status config.log yaws.pc \
	test/support/include.mk test/support/include.sh 2> /dev/null

local_install: all
	(cd scripts && $(MAKE) local_install)


# Target for folks that want to build a proper OTP release
# to be used with regular OTP release management.
release:	vsn.mk include.mk yaws.rel.src all
	sed -e "s/%YAWS_VSN%/${YAWS_VSN}/g" \
	       -e "s/%ERTS_VSN%/${ERTS_VSN}/" \
	       -e "s/%KERNEL_VSN%/${KERNEL_VSN}/" \
	       -e "s/%STDLIB_VSN%/${STDLIB_VSN}/" \
	       -e "s/%SASL_VSN%/${SASL_VSN}/" \
	       -e "s/%MNESIA_VSN%/${MNESIA_VSN}/" \
	< yaws.rel.src > yaws-${YAWS_VSN}.rel
	erlc -pa ./ebin yaws-${YAWS_VSN}.rel
	erl -pa ./ebin -noinput -run systools make_tar yaws-${YAWS_VSN} \
	       -s erlang halt
	@echo "-------------------------------"
	@echo
	@echo "Best to rerun this in an Erlang shell to pick up include directory:"
	@echo
	@echo "$$ erl -pa ./ebin"
	@echo "1> systools:make_tar(\"yaws-${YAWS_VSN}\", [{dirs,[include,examples,src]}])."
	@echo
	@echo "--------------------------------"

touch:
	find . -name '*' -print | xargs touch -m
	find . -name '*.erl' -print | xargs touch -m

yaws.plt:
	dialyzer --build_plt -r ebin --output_plt yaws.plt \
	   -r $(ERLDIR)/lib/sasl-$(SASL_VSN) \
	   -r $(ERLDIR)/lib/kernel-$(KERNEL_VSN) \
	   -r $(ERLDIR)/lib/stdlib-$(STDLIB_VSN) \
	   -r $(ERLDIR)/lib/erts-$(ERTS_VSN)

# Not debug compiled, let's just ignore it
#   	   -r $(ERLDIR)/lib/ssl-$(SSL_VSN)

dialyzer:	yaws.plt
	-dialyzer -q --plt yaws.plt -r ebin > dialyzer_warnings
	diff -U0 known_dialyzer_warnings dialyzer_warnings

.PHONY: test
test: all
	cd test && $(MAKE) all setup test
