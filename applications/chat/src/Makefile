include ../include.mk

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug
else
DEBUG_FLAGS =
endif

#include ../vsn.mk
#VSN=$(YAWS_VSN)
MODULES=chat

HDR_FILES=

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) 

ERLC_FLAGS+=-W +debug_info $(DEBUG_FLAGS) -I../../../include -pa ../../chat

#
# Targets
#

all: $(EBIN_FILES) 

$(EBIN_FILES) : $(HDR_FILES)

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES)

install: all
	install -d $(CHATINSTALLDIR)
	install -d $(EBININSTALLDIR)
	(cd ../ebin; find . -name '*' -print | xargs tar cf - ) | (cd $(EBININSTALLDIR); tar xf - )
	(cd ../chat; find . -name '*' -print | xargs tar cf -  ) | (cd $(CHATINSTALLDIR); tar xf - )



tags:
	erl  -noshell -pa ../ebin -s chat_debug mktags

xref:
	erl -noshell -pa ../ebin -s chat_debug xref ../ebin

