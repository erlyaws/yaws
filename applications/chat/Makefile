SUBDIRS	= src
include ./include.mk


all debug clean install:
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) $@ ) || exit 1 ; fi ; \
	  done


touch:
	find . -name '*' -print | xargs touch -m
	find . -name '*.erl' -print | xargs touch -m




