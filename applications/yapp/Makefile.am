SUBDIRS = src ebin

EDOC_OPTS=[{def,{vsn,"@YAPP_VSN@"}}, {source_path, ["@srcdir@"]}, {packages, false}]

EXTRA_DIST = vsn.mk README priv examples

install-data-local: docs
	$(AM_V_at)$(INSTALL) -d $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_yapp)
	$(AM_V_at)(cd @srcdir@; tar cf - priv examples ) | (cd $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_yapp); tar xf - )
	$(AM_V_at)(cd @builddir@; tar cf - doc ) | (cd $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_yapp); tar xf - )
	$(AM_V_at)if test $(yappdir) != $(ERLANG_INSTALL_LIB_DIR_yapp); then	\
	  $(INSTALL) -d $(DESTDIR)$(libdir);					\
	  rm -f $(DESTDIR)$(libdir)/yapp;					\
	  $(LN_S) -f $(ERLANG_INSTALL_LIB_DIR_yapp) $(DESTDIR)$(libdir)/yapp;	\
	fi

uninstall-local:
	$(AM_V_at)rm -fr $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_yapp)/doc
	$(AM_V_at)rm -fr $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_yapp)/priv
	$(AM_V_at)rm -fr $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_yapp)/examples
	$(AM_V_at)[ ! -h $(yappdir) ] || rm -f $(DESTDIR)$(libdir)/yapp

docs:
	$(AM_V_at)$(MKDIR_P) doc
	$(AM_V_at)$(ERL) -noshell -run edoc_run application "'yapp'" '"."' '$(EDOC_OPTS)' -s init stop

clean-local:
	$(AM_V_at)rm -rf doc

# Local Variables:
#    tab-width: 8
# End:

