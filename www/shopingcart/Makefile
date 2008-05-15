include ../../include.mk

all:	shopcart.beam
	cp shopcart.beam ../../examples/ebin
debug:

install:	all
	cp shopcart.beam $(DESTDIR)$(VARDIR)/yaws/ebin

clean:
	$(RM) shopcart.beam
	$(RM) ../../examples/ebin/shopcart.beam

shopcart.beam:	shopcart.erl
	$(ERLC) shopcart.erl
