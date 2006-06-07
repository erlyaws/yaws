
SUBDIRS	=	c_src src man www/shopingcart www/code doc scripts
include ./include.mk


all debug clean:	
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) $@ ) || exit 1 ; fi ; \
	  done


install:	all 
	set -e ; \
	for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) $@ ) || exit 1 ; fi ; \
	  done
	@echo "-------------------------------"
	@echo
	@echo "** etc files went into        ${ETCDIR}"	
	@echo "** executables went into      ${prefix}/bin"		
	@echo "** library files went into    ${prefix}/lib/yaws"
	@echo "** var files went into        ${VARDIR}"
	@echo "** default docroot went into  ${VARDIR}/yaws/www"
	@echo
	@echo "--------------------------------"


docs:
	( cd doc && $(MAKE) docs )

conf_clean:
	-rm include.mk config.cache config.status config.log 2> /dev/null

local_install: all
	(cd scripts && $(MAKE) local_install)


touch:
	find . -name '*' -print | xargs touch -m
	find . -name '*.erl' -print | xargs touch -m




foo:
	@echo "-------------------------------"
	@echo
	@echo "** etc files will go into     ${ETCDIR}"	
	@echo "** executables will go into   ${prefix}/bin"		
	@echo "** library file will go into  ${prefix}/lib/yaws"
	@echo "** var files will go into     ${VARDIR}"
	@echo
	@echo "--------------------------------"

