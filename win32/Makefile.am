##
## Debian Linux mingw cross-compiler version:
## To install
# sudo apt-get install mingw32

EXTRA_DIST = README.txt README.developer license.xml \
	     yaws.conf img
# TODO: replace .sh and yaws.conf

bin_PROGRAMS = yaws
yaws_CFLAGS  = $(EXTRAINCLUDE)
yaws_LDFLAGS =
yaws_LDADD   =
yaws_SOURCES = yaws.c

install-data-local:
	$(AM_V_at)$(INSTALL) -m 644 @srcdir@/yaws.conf        $(DESTDIR)$(yawsdir)
	$(AM_V_at)$(INSTALL) -m 644 @srcdir@/README.txt       $(DESTDIR)$(yawsdir)
	$(AM_V_at)$(INSTALL) -m 644 @top_srcdir@/vsn.mk       $(DESTDIR)$(yawsdir)
	$(AM_V_at)$(INSTALL) -m 644 @top_srcdir@/LICENSE      $(DESTDIR)$(yawsdir)
	$(AM_V_at)cp -r @top_srcdir@/ssl $(DESTDIR)$(yawsdir)

uninstall-local:
	$(AM_V_at)rm -fr $(DESTDIR)$(yawsdir)/ssl
	$(AM_V_at)rm -f $(DESTDIR)$(yawsdir)/yaws.conf	\
	  $(DESTDIR)$(yawsdir)/README.txt		\
	  $(DESTDIR)$(yawsdir)/LICENSE 			\
	  $(DESTDIR)$(yawsdir)/vsn.mk

mkinstaller: install
	$(AM_V_at)[ -n $(UNIX2DOS) ] && CONV=$(UNIX2DOS) || CONV=$(TODOS);		\
	$${CONV} $(DESTDIR)$(yawsdir)/LICENSE 2>/dev/null;				\
	for ext in txt erl conf html yaws js pem; do					\
	  find $(DESTDIR)$(yawsdir) -name "*.$${ext}" -exec $${CONV} {} \; 2>/dev/null;\
	done

	$(AM_V_at)$(INSTALL) -m 644 @builddir@/build.xml $(DESTDIR)$(yawsdir)
	$(AM_V_at)(cd $(DESTDIR)$(yawsdir) && $(INSTALL_BUILDER)/bin/builder build build.xml windows)
	$(AM_V_at)mv $(INSTALL_BUILDER)/output/*.exe @top_srcdir@
	$(AM_V_at)rm -f $(DESTDIR)$(yawsdir)/build.xml

# Local Variables:
#    tab-width: 8
# End:
