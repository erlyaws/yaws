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


cvstag:
	. vsn.mk; \
	CVS_RSH=ssh; \
	Y=`echo $$(YAWS_VSN) | sed 's/./-/g'` ;\
	cvs tag yaws-$$(Y) . ;\
	echo "TAGGED $$(Y)"


tar:
	. vsn.mk; \
	CVS_RSH=ssh; \
	Y=`echo $$(YAWS_VSN) | sed 's/./-/g'` ;\
	[ -d tmp ] && rm -rf tmp ; \
	(cd ..; cvs export -d tmp .; cd tmp; mv yaws yaws-$$(YAWS_VSN); \
	 tar cfz  yaws-$$(YAWS_VSN).tar.gz  yaws-$$(YAWS_VSN)  \;
	mv yaws-$$(YAWS_VSN) .. )




