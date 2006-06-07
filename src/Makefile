include ../include.mk

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug +debug_info
else
DEBUG_FLAGS =
endif

include ../vsn.mk
VSN=$(YAWS_VSN)
MODULES=yaws \
	yaws_app \
	yaws_ticker \
	yaws_config \
	yaws_server \
	yaws_sup \
	yaws_api \
	yaws_log \
	yaws_ls \
	yaws_debug \
	yaws_compile \
	yaws_ctl \
	yaws_cgi \
	yaws_zlib \
	yaws_generated \
	mime_type_c \
	mime_types \
	yaws_session_server \
	yaws_404 \
	yaws_revproxy \
	yaws_html  \
	yaws_log_file_h \
	yaws_rss \
	yaws_dav \
	yaws_pam \
	json jsonrpc yaws_jsonrpc yaws_xmlrpc


EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/yaws.app
ERLC_FLAGS+=-W $(DEBUG_FLAGS) -pa ../../yaws

#
# Targets
#

all: yaws_generated.erl $(EBIN_FILES) 

dav:
	$(MAKE) MODULES="$(MODULES) yaws_dav"

$(EBIN_FILES) : ../include/yaws.hrl ../include/yaws_api.hrl


yaws_generated.erl:	yaws_generated.template ../vsn.mk
	. ../vsn.mk; \
	sed -e "s/%VSN%/${YAWS_VSN}/" -e \
	       "s;%VARDIR%;${VARDIR};g"  -e  \
	       "s;%ETCDIR%;${ETCDIR};g"   \
	< yaws_generated.template > yaws_generated.erl



charset.def:	
	if [ ! -z "$(DEFAULT_CHARSET)" ]; then \
           echo  $(DEFAULT_CHARSET) > charset.def; \
	else rm charset.def 2> /dev/null; touch charset.def; fi

mime_types.erl:	mime.types mime_type_c.erl charset.def
	$(ERL) -noshell  -pa ../ebin -s mime_type_c  compile

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES) yaws_generated.erl charset.def mime_types.erl

install: all docsinstall
	install -d $(DESTDIR)/$(PREFIX)/lib/yaws/examples/ebin
	install -d $(DESTDIR)/$(PREFIX)/lib/yaws/examples/include
	install -d $(DESTDIR)/$(PREFIX)/lib/yaws/ebin
	install -d $(DESTDIR)/$(VARDIR)/log/yaws
	install -d $(DESTDIR)/$(VARDIR)/yaws/ebin
	cp -r ../include/* $(DESTDIR)/$(PREFIX)/lib/yaws/
	(cd ..; tar cf - ebin ) | (cd $(DESTDIR)/$(PREFIX)/lib/yaws; tar xf - )
	(cd ..; tar cf - include ) | (cd $(DESTDIR)/$(PREFIX)/lib/yaws; tar xf - )

docsinstall:
	install -d $(DESTDIR)/$(VARDIR)/yaws/www
	(cd ..; tar cf - www ) | (cd $(DESTDIR)/$(VARDIR)/yaws; tar xf - )



tags:
	erl  -noshell -pa ../ebin -s yaws_debug mktags

xref:
	erl -noshell -pa ../ebin -s yaws_debug xref ../ebin

DIALYZER_FLAGS+=-Wno_return

dialyzer: .dialyzer.ok

.dialyzer.ok:	$(MODULES:%=../ebin/%.$(EMULATOR))
	dialyzer  $(DIALYZER_FLAGS) -c ../ebin
	touch .dialyzer.ok
