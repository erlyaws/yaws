SUBDIRS = src ebin

EXTRA_DIST = wiki.conf start.sh README www

INSTDIR=$(DESTDIR)$(appsdir)/wiki
install-data-local:
	$(AM_V_at)$(INSTALL) -d $(INSTDIR)
	$(AM_V_at)(cd @srcdir@; tar cf - www ) | (cd $(INSTDIR); tar xf - )
	$(AM_V_at)chmod -R u+rw $(INSTDIR)/www

uninstall-local:
	$(AM_V_at)rm -fr $(INSTDIR)/www
