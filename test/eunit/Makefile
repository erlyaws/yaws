include ../support/include.mk

OBJ :=	multipart_post_parsing.beam \
	yaws_session_server_test.beam \
	ehtml_test.beam \
	cookies.beam \
	headers.beam \
	ipmask.beam

all:	$(OBJ)

test: all
	$(ERL) -pa ../src $(YTOP)/ebin -noshell -s test run $(OBJ:.beam=)

clean:	tclean

# needed because target is called by top-level test/Makefile
conf:
