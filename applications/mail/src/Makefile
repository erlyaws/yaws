include ../include.mk

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug
else
DEBUG_FLAGS =
endif

include ../vsn.mk
VSN=$(YAWS_VSN)
MODULES=mail attachment smtp mail_html

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) 

ERLC_FLAGS+=-W +debug_info $(DEBUG_FLAGS) -I../../../include -pa ../../mail

#
# Targets
#

all: $(EBIN_FILES) 

mail_vsn.erl:	mail_vsn.template
	. ../vsn.mk; sed "s/%VSN%/${MAIL_VSN}/" < mail_vsn.template > mail_vsn.erl

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES) mail_vsn.erl

install: all
	install -d $(MAILINSTALLDIR)
	install -d $(EBININSTALLDIR)
	install -d $(DESTDIR)$(PREFIX)/etc/mail
	@if [ -f $(DESTDIR)$(PREFIX)/etc/mail/yaws-webmail.conf ]; \
		then echo "$(DESTDIR)$(PREFIX)/etc/mail/yaws-webmail.conf already exists"; \
	else \
		cp yaws-webmail.conf $(DESTDIR)$(PREFIX)/etc/mail/yaws-webmail.conf; fi
	(cd ../ebin; find . -name '*' -print | xargs tar cf - ) | (cd $(EBININSTALLDIR); tar xf - )
	(cd ../mail; find . -name '*' -print | xargs tar cf -  ) | (cd $(MAILINSTALLDIR); tar xf - )



tags:
	erl  -noshell -pa ../ebin -s mail_debug mktags

xref:
	erl -noshell -pa ../ebin -s mail_debug xref ../ebin

