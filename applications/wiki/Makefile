SUBDIRS	= src scripts
include ./include.mk
include ./vsn.mk

all debug clean install:
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) $@ ) || exit 1 ; fi ; \
	  done


release: all
	install -d build/wiki-$(WIKI_VSN)/ebin
	install -d build/wiki-$(WIKI_VSN)/src
	install -d build/wiki-$(WIKI_VSN)/include
	install -d build/wiki-$(WIKI_VSN)/logs
	install -d build/wiki-$(WIKI_VSN)/scripts
	install -d build/wiki-$(WIKI_VSN)/yaws/WikiPreferences.files
	install -m 0666 ebin/*.beam build/wiki-$(WIKI_VSN)/ebin
	install -m 0666 src/*.erl build/wiki-$(WIKI_VSN)/src
	install -m 0666 src/Makefile build/wiki-$(WIKI_VSN)/src
	install -m 0666 wiki/*.{yaws,wob} build/wiki-$(WIKI_VSN)/yaws
	install -m 0666 wiki/WikiPreferences.files/*.{gif,png,css} build/wiki-$(WIKI_VSN)/yaws/WikiPreferences.files
	install -m 0755 scripts/getPassword build/wiki-$(WIKI_VSN)/scripts
	install -m 0755 scripts/importFiles build/wiki-$(WIKI_VSN)/scripts
	install -m 0755 scripts/updateWiki build/wiki-$(WIKI_VSN)/scripts
	install -m 0666 scripts/*.src build/wiki-$(WIKI_VSN)/scripts
	install -m 0666 scripts/Makefile build/wiki-$(WIKI_VSN)/scripts
	install -m 0666 *.mk build/wiki-$(WIKI_VSN)/
	install -m 0666 Makefile build/wiki-$(WIKI_VSN)/
	install -m 0755 start.sh build/wiki-$(WIKI_VSN)/
	install -m 0666 wiki.conf build/wiki-$(WIKI_VSN)/
	install -m 0666 README build/wiki-$(WIKI_VSN)/
	(cd build; tar zcvf wiki-$(WIKI_VSN).tar.gz wiki-$(WIKI_VSN))

touch:
	find . -name '*' -print | xargs touch -m
	find . -name '*.erl' -print | xargs touch -m




