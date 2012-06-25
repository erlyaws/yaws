include support/include.mk

SUBDIRS = t1 t2 t3 t4 t5 t6 eunit

all:	ibrowse
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
	nvsn=`git ls-remote -h $(IBROWSE_URI) master 2>/dev/null` ; \
	git_ok=$$? ; \
	set -e ; \
	if [ -f $(IBROWSE_VSN) -a $$git_ok -eq 0 ] ; then \
	  nvsn=`echo $$nvsn | awk '{print $$1}'` ; \
	  vsn=`cat $(IBROWSE_VSN)` ; \
	  [ "$$nvsn" = "$$vsn" ] || fetch=yes ; \
	else \
	  if [ ! -f $(IBROWSE_VSN) -a $$git_ok -ne 0 ] ; then \
	    echo error: ibrowse repository $(IBROWSE_URI) unreachable ; \
	    exit 1 ; \
	  fi ; \
	  [ $$git_ok -eq 0 ] && fetch=yes ; \
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
