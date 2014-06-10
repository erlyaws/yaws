bindir = $(ERLANG_INSTALL_LIB_DIR_yaws)/priv
libdir = $(ERLANG_INSTALL_LIB_DIR_yaws)/priv/lib

if WITH_EPAM
  bin_PROGRAMS = epam
  epam_CFLAGS  = $(EPAM_CFLAGS) $(EXTRAINCLUDE) -I$(ERLANG_ERTS_DIR)/include
  epam_LDFLAGS =
  epam_LDADD   =
  epam_SOURCES = epam.c
endif

lib_LTLIBRARIES = setuid_drv.la

setuid_drv_la_CFLAGS  = $(EXTRAINCLUDE) -I$(ERLANG_ERTS_DIR)/include
setuid_drv_la_LDFLAGS =  -module  -avoid-version
setuid_drv_la_SOURCES = setuid_drv.c

if HAVE_SENDFILE
  lib_LTLIBRARIES += yaws_sendfile_drv.la
  yaws_sendfile_drv_la_CFLAGS  = -DHAVE_SENDFILE $(EXTRAINCLUDE) -I$(ERLANG_ERTS_DIR)/include
  yaws_sendfile_drv_la_LDFLAGS = -module  -avoid-version
  yaws_sendfile_drv_la_SOURCES = yaws_sendfile_drv.c hashtable.c hashtable.h hashtable_private.h
endif

all-local: post-build

clean-local:
	$(AM_V_at)rm -f @top_builddir@/priv/epam
	$(AM_V_at)rm -fr @top_builddir@/priv/lib

post-build: $(bin_PROGRAMS) $(lib_LTLIBRARIES)
	$(AM_V_at)$(INSTALL) -d @top_builddir@/priv/lib
	$(AM_V_at)$(INSTALL) -m 644 epam @top_builddir@/priv
	$(AM_V_at)$(INSTALL) -m 644 .libs/setuid_drv.so @top_builddir@/priv/lib
	$(AM_V_at)$(INSTALL) -m 644 .libs/yaws_sendfile_drv.so @top_builddir@/priv/lib

# Local Variables:
#    tab-width: 8
# End:
