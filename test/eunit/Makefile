include ../support/include.mk

all:	multipart_post_parsing.beam

test:
	$(ERL) -pa ../src $(YTOP)/ebin -noshell -s test run multipart_post_parsing

clean:	tclean

# needed because target is called by top-level test/Makefile
conf: