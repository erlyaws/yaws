include ../support/include.mk

all:	multipart_post_parsing.beam \
	yaws_session_server_test.beam \
	ehtml_test.beam \
	embedded_yaws_id_dir.beam

test: all
	$(ERL) -pa ../src $(YTOP)/ebin -noshell \
	-s test run multipart_post_parsing yaws_session_server_test \
	ehtml_test embedded_yaws_id_dir

clean:	tclean

# needed because target is called by top-level test/Makefile
conf:
