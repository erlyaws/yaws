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
	yaws_trace \
	yaws_logger \
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
	yaws_outmod \
	yaws_revproxy \
	yaws_html  \
	yaws_log_file_h \
	yaws_rss \
	yaws_appmod_dav \
	yaws_runmod_lock \
	yaws_pam \
	json json2 jsonrpc yaws_jsonrpc yaws_xmlrpc\
	haxe yaws_rpc \
	yaws_soap_srv yaws_soap_lib yaws_soap12_lib \
	authmod_gssapi \
	yaws_appmod_cgi \
	yaws_appmod_fcgi \
	yaws_sendfile \
	yaws_sup_restarts \
	yaws_stats \
	yaws_vdir \
	yaws_multipart \
	yaws_shaper \
	yaws_dime \
	yaws_exhtml \
	yaws_sse \
	$(BITSMODS)



EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/yaws.app ../ebin/yaws.appup
ERLC_FLAGS+=-Werror $(DEBUG_FLAGS) -pa ../../yaws -I ../include

#
# Targets
#
all: yaws_generated.erl yaws_configure.hrl $(EBIN_FILES)

dav:
	$(MAKE) MODULES="$(MODULES) yaws_appmod_dav yaws_runmod_lock"

$(EBIN_FILES) : ../include/yaws.hrl ../include/yaws_api.hrl

../ebin/yaws_sendfile.$(EMULATOR): yaws_configure.hrl

yaws_generated.erl:	yaws_generated.template ../vsn.mk
	$(MAKE) IS_LOCAL_INSTALL=true gen_yaws_generated

yaws_configure.hrl:	 ../vsn.mk ../include.mk
	if [  $(HAVE_YAWS_SENDFILE) = "true" ]; then \
		echo "-define(HAVE_YAWS_SENDFILE, true)." > yaws_configure.hrl; \
	else touch yaws_configure.hrl; fi

mime_types.erl: ../priv/charset.def ../priv/mime.types \
				../ebin/yaws_generated.$(EMULATOR) ../ebin/mime_type_c.$(EMULATOR)
	$(ERL) -noshell -pa ../ebin -s mime_type_c generate


debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES) yaws_generated.erl yaws_configure.hrl mime_types.erl

install: all docsinstall
	$(MAKE) IS_LOCAL_INSTALL=false gen_yaws_generated
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/yaws/examples/ebin
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/yaws/examples/include
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/yaws/ebin
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/yaws/priv
	$(INSTALL) -d $(DESTDIR)$(VARDIR)/log/yaws
	$(INSTALL) -d $(DESTDIR)$(VARDIR)/yaws/ebin
	(cd ..; tar cf - ebin ) | (cd $(DESTDIR)$(LIBDIR)/yaws; tar xf - )
	(cd ..; tar cf - include ) | (cd $(DESTDIR)$(LIBDIR)/yaws; tar xf - )
	(cd ..; tar cf - priv/*.xsd ) | (cd $(DESTDIR)$(LIBDIR)/yaws; tar xf - )
	$(INSTALL) -m 644 ../priv/mime.types  $(DESTDIR)$(LIBDIR)/yaws/priv
	$(MAKE) IS_LOCAL_INSTALL=true gen_yaws_generated

docsinstall:
	$(INSTALL) -d $(DESTDIR)$(VARDIR)/yaws/www
	(cd ..; tar cf - www ) | (cd $(DESTDIR)$(VARDIR)/yaws; tar xf - )

tags:
	erl  -noshell -pa ../ebin -s yaws_debug mktags

xref:
	erl -noshell -pa ../ebin -s yaws_debug xref ../ebin



## need to differentiate between local install and not
gen_yaws_generated:
	rm -f yaws_generated.erl ../ebin/yaws_generated.$(EMULATOR)
	YAWS_VSN='$(YAWS_VSN)' VARDIR='$(VARDIR)' ETCDIR='$(ETCDIR)' \
	    ../scripts/gen-yaws-generated $(IS_LOCAL_INSTALL) > yaws_generated.erl
	$(MAKE) ../ebin/yaws_generated.$(EMULATOR)

