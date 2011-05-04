include ../include.mk

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug +debug_info
else
DEBUG_FLAGS =
endif

include ../vsn.mk
VSN=$(YAWS_VSN)


## the websocket code uses new modern bits stuff
ifeq ($(BITS_SUPPORT),true)
BITSMODS=yaws_websockets
endif


MODULES=yaws \
	yaws_app \
	yaws_ticker \
	yaws_config \
	yaws_server \
	yaws_sup \
	yaws_api \
	yaws_log \
	yaws_logger \
	yaws_ls \
	yaws_debug \
	yaws_compile \
	yaws_ctl \
	yaws_cgi \
	yaws_zlib \
	yaws_generated \
	mime_types \
	yaws_session_server \
	yaws_outmod \
	yaws_revproxy \
	yaws_html  \
	yaws_log_file_h \
	yaws_rss \
	yaws_dav \
	yaws_pam \
	json json2 jsonrpc yaws_jsonrpc yaws_xmlrpc\
	haxe yaws_rpc \
	yaws_soap_srv yaws_soap_lib \
	authmod_gssapi \
	yaws_appmod_cgi \
	yaws_appmod_fcgi \
	yaws_sendfile \
	yaws_sup_restarts \
	yaws_stats \
	yaws_vdir \
	yaws_multipart \
	yaws_shaper \
	$(BITSMODS)



EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/yaws.app ../ebin/yaws.appup
ERLC_FLAGS+=-W $(DEBUG_FLAGS) -pa ../../yaws -I ../include

#
# Targets
#

all: yaws_generated.erl yaws_configure.hrl $(EBIN_FILES)

dav:
	$(MAKE) MODULES="$(MODULES) yaws_dav"

$(EBIN_FILES) : ../include/yaws.hrl ../include/yaws_api.hrl

../ebin/yaws_sendfile.$(EMULATOR): yaws_configure.hrl

yaws_generated.erl:	yaws_generated.template ../vsn.mk
	. ../vsn.mk ; \
	YAWS_VSN='$(YAWS_VSN)' VARDIR='$(VARDIR)' ETCDIR='$(ETCDIR)' \
	    ../scripts/gen-yaws-generated true > yaws_generated.erl

yaws_configure.hrl:	 ../vsn.mk ../include.mk
	set -x; \
	if [  $(HAVE_SENDFILE) = "true" ]; then \
		echo "-define(HAVE_SENDFILE, true)." > yaws_configure.hrl; \
	else touch yaws_configure.hrl; fi


## need to differentiate between local install and not
.PHONY: regen
regen:
	. ../vsn.mk ; \
	YAWS_VSN='$(YAWS_VSN)' VARDIR='$(VARDIR)' ETCDIR='$(ETCDIR)' \
	    ../scripts/gen-yaws-generated false > yaws_generated.erl

charset.def:
	if [ -n "$(DEFAULT_CHARSET)" ]; then \
           echo $(DEFAULT_CHARSET) > charset.def; \
	else rm -f charset.def ; touch charset.def; fi

mime_type_c.beam: mime_type_c.erl

mime_types.erl:	mime.types charset.def mime_type_c.beam
	$(ERL) -noshell -pa ../ebin -s mime_type_c compile

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES) yaws_generated.erl charset.def \
		mime_types.erl yaws_configure.hrl

install: regen all docsinstall
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/lib/yaws/examples/ebin
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/lib/yaws/examples/include
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/lib/yaws/ebin
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/lib/yaws/priv
	$(INSTALL) -d $(DESTDIR)$(VARDIR)/log/yaws
	$(INSTALL) -d $(DESTDIR)$(VARDIR)/yaws/ebin
	(cd ..; tar cf - ebin ) | (cd $(DESTDIR)$(PREFIX)/lib/yaws; tar xf - )
	(cd ..; tar cf - include ) | (cd $(DESTDIR)$(PREFIX)/lib/yaws; tar xf - )
	(cd ..; tar cf - priv/*.xsd ) | (cd $(DESTDIR)$(PREFIX)/lib/yaws; tar xf - )

docsinstall:
	$(INSTALL) -d $(DESTDIR)$(VARDIR)/yaws/www
	(cd ..; tar cf - www ) | (cd $(DESTDIR)$(VARDIR)/yaws; tar xf - )

tags:
	erl  -noshell -pa ../ebin -s yaws_debug mktags

xref:
	erl -noshell -pa ../ebin -s yaws_debug xref ../ebin


