include support/include.mk

SUBDIRS = t1 t2 t3 eunit

all:	conf ibrowse
	@cd src; $(MAKE) all
	@set -e ; \
          for d in $(SUBDIRS) ; do \
            if [ -f $$d/Makefile ]; then \
              ( cd $$d && $(MAKE) conf && $(MAKE) $@ ) || exit 1 ; \
            fi ; \
          done

clean:
	@cd src; $(MAKE) clean
	rm -rf ibrowse
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

ibrowse: fetch_ibrowse
	@cd ibrowse; $(MAKE)

IBROWSE_URI = https://github.com/cmullaparthi/ibrowse.git
IBROWSE_DOWNLOAD_URI = https://github.com/cmullaparthi/ibrowse/tarball/master
IBROWSE_TGZ = ibrowse.tar.gz
IBROWSE_VSN = ibrowse/.version

.PHONY: fetch_ibrowse
fetch_ibrowse:
	set -e ; \
	nvsn=`git ls-remote -h $(IBROWSE_URI) master | awk '{print $$1}'` ; \
	if [ -f $(IBROWSE_VSN) ] ; then \
	  vsn=`cat $(IBROWSE_VSN)` ; \
	  [ "$$nvsn" = "$$vsn" ] || fetch=yes ; \
	else \
	  fetch=yes ; \
	fi ; \
	if [ -n "$$fetch" ] ; then \
	  rm -rf ibrowse ; \
	  curl -L -o $(IBROWSE_TGZ) $(IBROWSE_DOWNLOAD_URI) ; \
	  dir=`tar ztf $(IBROWSE_TGZ) | head -1 | sed -e 's/\/$$//'` ; \
	  tar zxf $(IBROWSE_TGZ) ; \
	  mv $$dir ibrowse ; \
	  rm -f $(IBROWSE_TGZ) ; \
	  echo $$nvsn > $(IBROWSE_VSN) ; \
	fi
