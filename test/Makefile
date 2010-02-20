include support/include.mk

SUBDIRS = t1 t2

all:	conf
	@cd ibrowse/src; $(MAKE)
	@cd src; $(MAKE) all
	@set -e ; \
          for d in $(SUBDIRS) ; do \
            if [ -f $$d/Makefile ]; then \
              ( cd $$d && $(MAKE) conf && $(MAKE) $@ ) || exit 1 ; \
            fi ; \
          done




clean: 	
	@cd src; $(MAKE) clean
	@cd ibrowse/src; $(MAKE) clean
	@set -e ; \
          for d in $(SUBDIRS) ; do \
            if [ -f $$d/Makefile ]; then \
              ( cd $$d && $(MAKE) $@ ) || exit 1 ; \
            fi ; \
          done

test:
	@set -e ; \
          for d in $(SUBDIRS) ; do \
            if [ -f $$d/Makefile ]; then \
              ( cd $$d && $(MAKE) all start test ) || exit 1 ; \
            fi ; \
          done
