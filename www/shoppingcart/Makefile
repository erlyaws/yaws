include ../../include.mk

all:	shopcart.beam
	cp -f shopcart.beam ../../examples/ebin 
debug:

install:	all
	cp -f shopcart.beam $(DESTDIR)$(VARDIR)/yaws/ebin

clean:
	$(RM) -f shopcart.beam
	$(RM) -f ../../examples/ebin/shopcart.beam

shopcart.beam:	shopcart.erl
	$(ERLC) shopcart.erl
