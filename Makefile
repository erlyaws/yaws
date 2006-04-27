
SUBDIRS	=	c_src src man www/shopingcart www/code doc scripts
include ./include.mk


all debug clean:	
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) $@ ) || exit 1 ; fi ; \
	  done


install:	all
	@yrun=`yaws --status 2> /dev/null`; \
	case $$yrun in \
		*Uptime*) \
			echo "ERROR Can't install while a yaws system is already running"; \
			exit;; \
		*) \
			true;; \
	esac; \
	set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) $@ ) || exit 1 ; fi ; \
	  done



docs:
	( cd doc && $(MAKE) docs )

conf_clean:
	-rm include.mk config.cache config.status config.log 2> /dev/null

local_install: all
	(cd scripts && $(MAKE) local_install)


touch:
	find . -name '*' -print | xargs touch -m
	find . -name '*.erl' -print | xargs touch -m




