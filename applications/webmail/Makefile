include ../../include.mk

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug
else
DEBUG_FLAGS =
endif

include ../../vsn.mk
VSN=$(YAWS_VSN)

MODULES=pop3lib_cli \
	smtp_client \
	webmail


EBIN_FILES=$(MODULES:%=%.$(EMULATOR)) 
ERLC_FLAGS+=-W +debug_info $(DEBUG_FLAGS) -pa ../../yaws

#
# Targets
#



all: $(EBIN_FILES) 

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES) 

install: all
	install -d $(INSTALLPREFIX)/lib/yaws/applications/webmail/ebin
	install -d $(INSTALLPREFIX)/lib/yaws/applications/webmail/www
	(tar czv  *.beam  --) | (cd $(INSTALLPREFIX)/lib/yaws/applications/webmail/ebin; tar xz -- )
	(tar czv  *.gif *.yaws *.css  --) | (cd $(INSTALLPREFIX)/lib/yaws/applications/webmail/www; tar xz -- )

