###-*-makefile-*-   ; force emacs to enter makefile-mode


# these the items can be edited

INSTALLPREFIX=/var/yaws

CHATINSTALLDIR=$(INSTALLPREFIX)/chat
EBININSTALLDIR=$(INSTALLPREFIX)/ebin

ERL=$(shell which erl)
ERLC=$(shell which erlc)


## don't edit below here

EMULATOR=beam
ifdef debug
  ERLC_FLAGS+=-Ddebug
endif

ifdef trace
  ERLC_FLAGS=+trace
endif

ifdef export_all
  ERLC_FLAGS+=-Dexport_all
endif


INSTALL=install -c
INSTALL_DATA=${INSTALL} -m 644


# Hmm, don't know if you are supposed to like this better... ;-)
APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

# Targets

../ebin/%.app: %.app.src ../vsn.mk Makefile
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

../ebin/%.appup: %.appup 
	cp $< $@

../ebin/%.$(EMULATOR): %.erl
	$(ERLC) -b $(EMULATOR) $(ERLC_FLAGS) -o ../ebin $<

%.$(EMULATOR): %.erl
	$(ERLC) -b $(EMULATOR) $(ERLC_FLAGS) $<



