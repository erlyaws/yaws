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
	yaws_ctl

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/yaws.app

ERLC_FLAGS+=-W $(DEBUG_FLAGS) -pa ../../yaws

#
# Targets
#



all: $(EBIN_FILES) 

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES) 

install: all
	install -d $(INSTALLPREFIX)/lib/yaws/examples/ebin
	install -d $(INSTALLPREFIX)/lib/yaws/examples/include
	install -d $(INSTALLPREFIX)/lib/yaws/ebin
	install -d /var/log/yaws
	install -d /var/yaws/www
	install -d /val/yaws/ebin
	(cd ..; tar cz  ebin  --) | (cd $(INSTALLPREFIX)/lib/yaws; tar xz -- )
	(cd ..; tar cz  include  --) | (cd $(INSTALLPREFIX)/lib/yaws; tar xz -- )
	(cd ..; tar cz  www  --) | (cd /var/yaws; tar xz -- )



tags:
	erl  -noshell -pa ../ebin -s yaws mktags
