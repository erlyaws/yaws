include ../../include.mk

all:	shopcart.beam
	cp shopcart.beam ../../examples/ebin
debug:

install:	all
	cp shopcart.beam $(VARDIR)/yaws/ebin

clean:
	$(RM) shopcart.beam
	$(RM) ../../examples/ebin/shopcart.beam

shopcart.beam:	shopcart.erl
	erlc shopcart.erl
