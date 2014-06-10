MANPAGES = yaws.1 yaws.conf.5 yaws_api.5 yaws_soap_lib.5
PDFS     = $(MANPAGES:=.pdf)

EXTRA_DIST = $(MANPAGES)

if !MINGW

install-data-local:
	$(AM_V_at)$(INSTALL) -d $(DESTDIR)$(mandir)/man1
	$(AM_V_at)$(INSTALL) -d $(DESTDIR)$(mandir)/man5
	$(AM_V_at)$(INSTALL) -m 644 @srcdir@/yaws.1          $(DESTDIR)$(mandir)/man1
	$(AM_V_at)$(INSTALL) -m 644 @srcdir@/yaws.conf.5     $(DESTDIR)$(mandir)/man5
	$(AM_V_at)$(INSTALL) -m 644 @srcdir@/yaws_api.5      $(DESTDIR)$(mandir)/man5
	$(AM_V_at)$(INSTALL) -m 644 @srcdir@/yaws_soap_lib.5 $(DESTDIR)$(mandir)/man5

uninstall-local:
	$(AM_V_at)rm -f $(DESTDIR)$(mandir)/man1/yaws.1
	$(AM_V_at)rm -f $(DESTDIR)$(mandir)/man5/yaws.conf.5
	$(AM_V_at)rm -f $(DESTDIR)$(mandir)/man5/yaws_api.5
	$(AM_V_at)rm -f $(DESTDIR)$(mandir)/man5/yaws_soap_lib.5
else

install-data-local: pdf
	$(AM_V_at)$(INSTALL) -d $(DESTDIR)$(mandir)
	$(AM_V_at)$(INSTALL) -m 644 yaws.1.pdf          $(DESTDIR)$(mandir)
	$(AM_V_at)$(INSTALL) -m 644 yaws.conf.5.pdf     $(DESTDIR)$(mandir)
	$(AM_V_at)$(INSTALL) -m 644 yaws_api.5.pdf      $(DESTDIR)$(mandir)
	$(AM_V_at)$(INSTALL) -m 644 yaws_soap_lib.5.pdf $(DESTDIR)$(mandir)

uninstall-local:
	$(AM_V_at)rm -f $(DESTDIR)$(mandir)/yaws.1.pdf
	$(AM_V_at)rm -f $(DESTDIR)$(mandir)/yaws.conf.5.pdf
	$(AM_V_at)rm -f $(DESTDIR)$(mandir)/yaws_api.5.pdf
	$(AM_V_at)rm -f $(DESTDIR)$(mandir)/yaws_soap_lib.5.pdf

## for the win32 build, the rest of us know how to run man
pdf-local: $(PDFS)

%.pdf: %
	$(AM_V_GEN)$(MAN) -t $< 2>/dev/null | $(PS2PDF) - $@;

clean-local:
	$(AM_V_at)rm -f $(PDFS)

endif
# Local Variables:
#    tab-width: 8
# End:
