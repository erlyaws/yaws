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
	yaws_ssl \
	yaws_vsn \
	mime_type_c \
	mime_types \
	yaws_session_server \
	yaws_404 \
	yaws_revproxy \
	yaws_html 



EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/yaws.app
ERLC_FLAGS+=-W $(DEBUG_FLAGS) -pa ../../yaws

#
# Targets
#

all: yaws_vsn.erl $(EBIN_FILES) 

$(EBIN_FILES) : ../include/yaws.hrl ../include/yaws_api.hrl

yaws_vsn.erl:	yaws_vsn.template
	. ../vsn.mk; sed "s/%VSN%/${YAWS_VSN}/" < yaws_vsn.template > yaws_vsn.erl

mime_types.erl:	mime.types mime_type_c.erl
	erl -noshell  -pa ../ebin -s mime_type_c  compile

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES) yaws_vsn.erl

install: all docsinstall
	install -d $(INSTALLPREFIX)/lib/yaws/examples/ebin
	install -d $(INSTALLPREFIX)/lib/yaws/examples/include
	install -d $(INSTALLPREFIX)/lib/yaws/ebin
	install -d $(VARDIR)/log/yaws
	install -d $(VARDIR)/yaws/ebin
	(cd ..; tar cf - ebin ) | (cd $(INSTALLPREFIX)/lib/yaws; tar xf - )
	(cd ..; tar cf - include ) | (cd $(INSTALLPREFIX)/lib/yaws; tar xf - )

docsinstall:
	install -d $(VARDIR)/yaws/www
	(cd ..; tar cf - www ) | (cd $(VARDIR)/yaws; tar xf - )



tags:
	erl  -noshell -pa ../ebin -s yaws_debug mktags

xref:
	erl -noshell -pa ../ebin -s yaws_debug xref ../ebin

