SUBDIRS	=	c_src src scripts man www/shopingcart doc
APPS = webmail
include ./include.mk


all debug clean install:	
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) $@ ) || exit 1 ; fi ; \
	  done



conf_clean:
	-rm include.mk config.cache config.status config.log 2> /dev/null

local_install: all
	(cd scripts && $(MAKE) local_install)

apps:
	for a in $(APPS) ; do
		if [ -f applications/$$a/Makefile ]; then \
			(cd applications/$$a && $(MAKE) ) || exit 1; fi ; \
	done



touch:
	find . -name '*' -print | xargs touch -m
	find . -name '*.erl' -print | xargs touch -m




