include ../include.mk

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug
else
DEBUG_FLAGS =
endif

include ../vsn.mk
VSN=$(YAWS_VSN)
MODULES=yaws \
	yaws_app \
	yaws_config \
	yaws_server \
	yaws_sup \
	yaws_api \
	yaws_log \
	yaws_ls \
	yaws_debug \
	yaws_compile \
	yaws_ctl \
	yaws_ssl \
	yaws_vsn \
	mime_type_c \
	mime_types \
	yaws_session_server

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/yaws.app

ERLC_FLAGS+=-W +debug_info $(DEBUG_FLAGS) -pa ../../yaws

#
# Targets
#



all: yaws_vsn.erl $(EBIN_FILES) 

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
	install -d /var/log/yaws
	install -d /var/yaws/ebin
	(cd ..; tar cf - ebin ) | (cd $(INSTALLPREFIX)/lib/yaws; tar xf - )
	(cd ..; tar cf - include ) | (cd $(INSTALLPREFIX)/lib/yaws; tar xf - )

docsinstall:
	install -d /var/yaws/www
	(cd ..; tar cf - www ) | (cd /var/yaws; tar xf - )



tags:
	erl  -noshell -pa ../ebin -s yaws_debug mktags

xref:
	erl -noshell -pa ../ebin -s yaws_debug xref ../ebin

