ERL_FILES = ibrowse.erl \
		ibrowse_http_client.erl \
		ibrowse_app.erl \
		ibrowse_sup.erl \
		ibrowse_lib.erl \
		ibrowse_lb.erl \
		ibrowse_test.erl


INCLUDE_DIRS = -I./ 

ERLC ?= erlc
ERLC_EMULATOR ?= erl -boot start_clean
COMPILER_OPTIONS = -W +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

.SUFFIXES: .erl .beam $(SUFFIXES)
EBIN = ../ebin

all: $(ERL_FILES:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	${ERLC} $(COMPILER_OPTIONS) $(INCLUDE_DIRS) -o ../ebin $<

clean:
	rm -f $(EBIN)/*.beam
