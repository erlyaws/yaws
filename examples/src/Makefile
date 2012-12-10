include ../../include.mk

ERLC_FLAGS += -I../../include

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug +debug_info
else
DEBUG_FLAGS =
endif


MODULES= advanced_echo_callback \
	 	 authmod_gssapi \
		 basic_echo_callback \
		 basic_echo_callback_extended \
		 server_sent_events

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR))
ERLC_FLAGS+=-Werror $(DEBUG_FLAGS)

all: $(EBIN_FILES)

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES)

install: all
	cp -f ../ebin/*.beam $(DESTDIR)$(VARDIR)/yaws/ebin
