SUBDIRS	=	src scripts


all:
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) ) || exit 1 ; fi ; \
	  done

clean:
	@for d in $(SUBDIRS) ; do \
	  if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) clean ) fi ; \
	done


install:
	@for d in $(SUBDIRS) ; do \
	  if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) install ) fi ; \
	done	

touch:
	find . -name '*' -print | xargs touch -m
	find . -name '*.erl' -print | xargs touch -m


release:
	. vsn.mk; \
	CVS_RSH=ssh; \
	cvs tag yaws-$$(YAWS_VSN) . ;\
	
