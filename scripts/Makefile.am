include @top_srcdir@/include.mk

EXTRA_DIST = darwin debian freebsd gentoo netbsd openbsd redhat suse systemd 	\
	     gen-yaws gen-yaws-conf gen-yaws-generated Subst			\
	     regular-install make-release make-upload				\
	     rebar-pre-script yaws.conf.template yaws.template

if !MINGW

install-data-local: yaws_script yaws_conf yaws_init_script

uninstall-local:
	$(AM_V_at)if diff -U0 $(DESTDIR)$(confdir)/yaws.conf.template	\
	  $(DESTDIR)$(confdir)/yaws.conf; then				\
	    rm -f $(DESTDIR)$(confdir)/yaws.conf;			\
	else								\
	   echo "Keep modified config file";				\
	fi

	$(AM_V_at)rm -f $(DESTDIR)$(bindir)/yaws
	$(AM_V_at)rm -f $(DESTDIR)$(sysconfdir)/init.d/yaws
	$(AM_V_at)rm -f $(DESTDIR)$(sysconfdir)/rc.d/yaws
	$(AM_V_at)rm -f $(DESTDIR)$(sysconfdir)/systemd/system/yaws.service
	$(AM_V_at)rm -f $(DESTDIR)$(confdir)/yaws.conf.template

yaws_script:
	$(AM_V_GEN)
	$(AM_V_at)$(INSTALL) -d $(DESTDIR)$(bindir)
	$(AM_V_at)(cd @srcdir@ &&		\
	  YAWSDIR='$(yawsdir)'			\
	  VARDIR='$(localstatedir)'		\
	  ERLBINDIR='$(ERLANG_ERTS_DIR)/bin'	\
	  ERL='$(ERL)' WERL='$(WERL)' 		\
	    ./gen-yaws > $(DESTDIR)$(bindir)/yaws)
	$(AM_V_at)chmod +x $(DESTDIR)$(bindir)/yaws

yaws_conf:
	$(AM_V_GEN)
	$(AM_V_at)$(INSTALL) -d $(DESTDIR)$(confdir);
	$(AM_V_at)user=$${SUDO_USER:-$$(whoami)};			\
	if [ $${user} == 'root' -o $${user} != $$(whoami) ]; then	\
	  PORT=80;							\
	  SSLPORT=443;							\
	else								\
	  PORT=8000;							\
	  SSLPORT=4443;							\
	fi;								\
	(cd @srcdir@ &&							\
	  YAWSDIR='$(yawsdir)'						\
	  LOGDIR='$(logdir)'						\
	  VARDIR='$(localstatedir)'					\
	  PORT=$${PORT}							\
	  DOCROOT='$(wwwdir)'						\
	  CERTDIR='$(confdir)'						\
	  SSLPORT=$${SSLPORT}						\
	    ./gen-yaws-conf > $(DESTDIR)$(confdir)/yaws.conf.template)

	$(AM_V_at)if [ -f "$(DESTDIR)$(confdir)/yaws.conf" ]; then	\
	  echo "* Keeping old config file ";				\
	else								\
	  echo "* Installing new config file";				\
	  $(INSTALL) -m 644 $(DESTDIR)$(confdir)/yaws.conf.template 	\
	    $(DESTDIR)$(confdir)/yaws.conf;				\
	fi

yaws_init_script:
	$(AM_V_GEN)
	$(AM_V_at)(cd @srcdir@ &&			\
	  DESTDIR='$(DESTDIR)'				\
	  PREFIX='$(prefix)'				\
	  ETCDIR='$(sysconfdir)'			\
	  BINDIR='$(bindir)'				\
	  LOGDIR='$(logdir)'				\
	  YAWSDIR='$(ERLANG_INSTALL_LIB_DIR_yaws)'	\
	    ./regular-install)

changelog:
	$(AM_V_GEN)
	$(AM_V_at)DATE="$(shell date +%Y-%m-%d)";	\
	  TAG="yaws-@YAWS_VSN@";			\
	  FILE="$${DATE}-$${TAG}.inc";			\
	  (cd @srcdir@ &&				\
	    ./gitlog-to-changelog $${TAG}  > @top_srcdir@/www/changelogs/$${FILE})

news:
	$(AM_V_GEN)
	$(AM_V_at)DATE="$(shell date +%Y-%m-%d-%H%M%S)";\
	  TAG="yaws-@YAWS_VSN@";			\
	  FILE="$${DATE}-$${TAG}.inc";			\
	  (cd @srcdir@ &&				\
	    ./gen-release-news $${TAG}  > @top_srcdir@/www/news/$${FILE})

gh-contributors:
	$(AM_V_GEN)
	$(AM_V_at)git log --format='%aN' | sort -u >@top_srcdir@/www/$@.txt
endif

# Local Variables:
#    tab-width: 8
# End:
