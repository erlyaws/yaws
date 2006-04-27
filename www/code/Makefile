include ../../include.mk

all:	myappmod.beam
	cp myappmod.beam ../../examples/ebin
debug:

install:	all
	cp myappmod.beam $(DESTDIR)/$(VARDIR)/yaws/ebin

clean:
	$(RM) myappmod.beam
	$(RM) ../../examples/ebin/myappmod.beam

myappmod.beam:	myappmod.erl
	$(ERLC) myappmod.erl
