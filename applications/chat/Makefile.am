SUBDIRS = src ebin

INSTDIR=$(DESTDIR)$(appsdir)/chat

EXTRA_DIST = www

install-data-local:
	$(AM_V_at)$(INSTALL) -d $(INSTDIR)
	$(AM_V_at)(cd @srcdir@; tar cf - www ) | (cd $(INSTDIR); tar xf - )
	$(AM_V_at)chmod -R u+rw $(INSTDIR)/www

uninstall-local:
	$(AM_V_at)rm -fr $(INSTDIR)/www

# Local Variables:
#    tab-width: 8
# End:
