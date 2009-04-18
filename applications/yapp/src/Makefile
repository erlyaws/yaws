YAWSDIRECTORY=../../..
include ../vsn.mk
include $(YAWSDIRECTORY)/include.mk

VSN=$(YAPP_VSN)

DEBUG=

EINCLUDE=../include
YAWS_INCL=$(YAWSDIRECTORY)/include

ERLC_FLAGS+=-W $(DEBUG) -I$(EINCLUDE) -I$(YAWS_INCL) 


MODULES = yapp yapp_app yapp_sup yapp_handler yapp_server yapp_registry \
	yapp_mnesia_server yapp_event_handler yapp_ets_server
EBIN = ../ebin
EBIN_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(EBIN)/yapp.app

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG
clean:
	rm -rf $(EBIN_FILES)

