###-*-makefile-*-   ; force emacs to enter makefile-mode

INSTALLPREFIX      = /usr/local

ERL=/opt/otp/R8B_continue/bin/erl
ERLC=/opt/otp/R8B_continue/bin/erlc
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

CC=gcc
CFLAGS=-g -O2
LINKER=gcc
LDFLAGS=
WIN32=
BSDI=
EXE=
DLL=so
OBJ=o
OUT=-o 
FPIC=-fpic
STRIP=strip
LIBS=



ERLDIR=/opt/otp/R8B_continue/lib/erlang
ERL_INTERFACE_LIB=/opt/otp/R8B_continue/lib/erlang/lib/erl_interface-/lib
ERL_INTERFACE_LIBS=-lerl_interface -lei 
ERL_INTERFACE_INCLUDE=/opt/otp/R8B_continue/lib/erlang/lib/erl_interface-/include

LD_SHARED=ld -shared

LDFLAGS +=-L$(ERL_INTERFACE_LIB)

INSTALL=/usr/bin/install -c
INSTALL_DATA=${INSTALL} -m 644



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



