include ../include.mk

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug
else
DEBUG_FLAGS =
endif

include ../vsn.mk
VSN=$(YAWS_VSN)
MODULES=wiki \
	wiki_templates \
	wiki_split \
	wiki_to_html \
	wiki_format_txt \
	wiki_utils \
	wiki_diff \
	wiki_yaws \
	wiki_plugin_dummy \
	wiki_plugin_backlinks \
	wiki_plugin_menu \
	utils

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) 

ERLC_FLAGS+=-W +debug_info $(DEBUG_FLAGS) -I../../../include -pa ../../wiki

#
# Targets
#



all: $(EBIN_FILES) 

wiki_vsn.erl:	wiki_vsn.template
	. ../vsn.mk; sed "s/%VSN%/${WIKI_VSN}/" < wiki_vsn.template > wiki_vsn.erl

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES) wiki_vsn.erl

install: all
	install -d $(WIKIINSTALLDIR)
	install -d $(EBININSTALLDIR)
	(cd ../ebin; find . -name '*' -print | xargs tar cf - ) | (cd $(EBININSTALLDIR); tar xf - )
	(cd ../wiki; find . -name '*' -print | xargs tar cf -  ) | (cd $(WIKIINSTALLDIR); tar xf - )



tags:
	erl  -noshell -pa ../ebin -s wiki_debug mktags

xref:
	erl -noshell -pa ../ebin -s wiki_debug xref ../ebin

