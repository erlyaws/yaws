SUBDIRS = src ebin

EXTRA_DIST = yaws-webmail.conf TODO README www

INSTDIR=$(DESTDIR)$(appsdir)/mail

install-data-local:
	$(AM_V_at)$(INSTALL) -d $(DESTDIR)$(sysconfdir)/mail
	$(AM_V_at)if [ -f $(DESTDIR)$(sysconfdir)/mail/yaws-webmail.conf ]; then	\
	  echo "$(DESTDIR)$(sysconfdir)/mail/yaws-webmail.conf already exists"; 	\
	else										\
	  $(INSTALL) -m 644 @srcdir@/yaws-webmail.conf $(DESTDIR)$(sysconfdir)/mail;	\
	fi

	$(AM_V_at)$(INSTALL) -d $(INSTDIR)
	$(AM_V_at)(cd @srcdir@; tar cf - www ) | (cd $(INSTDIR); tar xf - )
	$(AM_V_at)chmod -R u+rw $(INSTDIR)/www

uninstall-local:
	$(AM_V_at)rm -f $(DESTDIR)$(sysconfdir)/mail/yaws-webmail.conf $(INSTDIR)/www

# Local Variables:
#    tab-width: 8
# End:
