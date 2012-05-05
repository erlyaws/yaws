YAWSDIRECTORY=../..
include vsn.mk
include $(YAWSDIRECTORY)/include.mk

VSN = $(YAPP_VSN)
APPNAME=yapp
DOC_OPTS=[{def,{vsn,"$(VSN)"}}]
INSTALL=$(YAWSDIRECTORY)/install-sh


all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)

appfile:
	(cd src;$(MAKE) ../ebin/$(APPNAME).app)

docs:
	$(ERL) -noshell  -run edoc_run application "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop

cleandocs:
	rm -rf doc/*.html doc/edoc-info doc/stylesheet.css doc/*.png

install: all docs
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/$(APPNAME)/ebin
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/$(APPNAME)/priv/docroot
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/$(APPNAME)/doc
	(tar cf - ebin priv doc) | (cd $(DESTDIR)$(LIBDIR)/yapp; tar xf - )

