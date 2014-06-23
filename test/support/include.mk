WAIT_TIME=10

ERL_GENERIC_FLAGS = -pa $(top_srcdir) -pa $(top_builddir)				\
		    -pa $(top_builddir)/ebin -pa $(top_builddir)/test			\
		    -pa $(top_builddir)/test/src -pa $(top_builddir)/test/ibrowse/ebin	\
		    -pa $(top_builddir)/c_src/.libs

# Override this variable if you want to change the yaws's configuration
YAWS_CONF ?= $(top_builddir)/test/support/yaws.conf

DEPS =  $(top_builddir)/test/src/tftest.beam		\
	$(top_builddir)/test/src/test.beam		\
	$(top_builddir)/priv/epam			\
	$(top_builddir)/priv/lib/setuid_drv.so		\
	$(abs_top_builddir)/priv/mime.types		\
	$(top_builddir)/ebin/yaws.beam


ifeq ($(HAVE_ERLANG_SENDFILE),true)
  DEPS += $(top_builddir)/priv/lib/yaws_sendfile_drv.so
endif

start: $(top_builddir)/test/bin/yaws quiet-stop prepare-test
	$(AM_V_at)$(top_builddir)/test/bin/yaws --sname test --daemon --id testid --conf $(YAWS_CONF)

wait-started: $(top_builddir)/test/bin/yaws
	$(AM_V_at)$(top_builddir)/test/bin/yaws --id testid --wait-started=$(WAIT_TIME)

wait-stopped: $(top_builddir)/test/bin/yaws
	$(AM_V_at)$(top_builddir)/test/bin/yaws --id testid --wait-stopped=$(WAIT_TIME)

stop: $(top_builddir)/test/bin/yaws
	$(AM_V_at)$(top_builddir)/test/bin/yaws --id testid --stop >/dev/null

quiet-stop: $(top_builddir)/test/bin/yaws
	$(AM_V_at)($(top_builddir)/test/bin/yaws --id testid --stop >/dev/null || true)
	$(AM_V_at)($(top_builddir)/test/bin/yaws --id testid --wait-stopped=$(WAIT_TIME) >/dev/null || true)

hup: $(top_builddir)/test/bin/yaws
	$(AM_V_at)$(top_builddir)/test/bin/yaws --id testid --hup >/dev/null

status: $(top_builddir)/test/bin/yaws
	$(AM_V_at)$(top_builddir)/test/bin/yaws --id testid --status >/dev/null

i: $(top_builddir)/test/bin/yaws
	$(AM_V_at)$(top_builddir)/test/bin/yaws --sname test -i --id testid --conf $(YAWS_CONF)

connect: $(top_builddir)/test/bin/yaws
	$(AM_V_at)$(ERL) -sname client -remsh test@`hostname`

test: check

app-test: $(DEPS) all start wait-started do-test stop wait-stopped

do-test:
	$(AM_V_at)$(ERL) -sname tftest -noinput $(ERL_FLAGS) -s tftest;	\
	  err=$$?;							\
	  if test $$err -ne 0; then					\
	    $(MAKE) quiet-stop;						\
	    cat logs/report.log;					\
	  fi;								\
	  exit $$err

app_test.beam: $(top_builddir)/test/ibrowse/include/ibrowse.hrl $(top_builddir)/test/ibrowse/ebin/ibrowse.beam

%.beam: %.erl
	$(AM_V_at)$(ERLC) $(ERLC_FLAGS) -M -MF $(@:%.beam=$(DEPDIR)/%.Pbeam) -MT $@ $<
	$(AM_V_ERLC)$(ERLC) $(ERLC_FLAGS) -o . $<


$(top_builddir)/test/bin/yaws:
	$(AM_V_at)$(INSTALL) -d $(top_builddir)/test/bin
	$(AM_V_GEN)(cd $(top_srcdir)/scripts &&		\
	  YAWSDIR='$(abs_top_builddir)'			\
	  VARDIR='$(abs_top_builddir)'			\
	  ERLBINDIR='$(ERLANG_ERTS_DIR)/bin'		\
	  ERL='$(ERL)' WERL='$(WERL)'			\
	    ./gen-yaws > $(abs_top_builddir)/test/bin/yaws)
	$(AM_V_at)chmod +x $(top_builddir)/test/bin/yaws

$(top_builddir)/test/ibrowse/ebin/ibrowse.beam \
$(top_builddir)/test/ibrowse/include/ibrowse.hrl:
	$(AM_V_at)$(MAKE) ibrowse

$(DEPS):
	@echo $@
	$(AM_V_at)(cd $(top_builddir) && $(MAKE));

ibrowse: fetch-ibrowse
	$(AM_V_at)(cd $(top_builddir)/test/ibrowse && $(ESCRIPT) ./rebar compile)

IBROWSE_URI          = https://github.com/cmullaparthi/ibrowse.git
IBROWSE_DOWNLOAD_URI = https://github.com/cmullaparthi/ibrowse/tarball/v3.0.4
IBROWSE_TGZ          = $(top_builddir)/test/ibrowseibrowse.tar.gz
IBROWSE_VSN          = $(top_builddir)/test/ibrowse/.version

fetch-ibrowse:
	$(AM_V_at)nvsn=`git ls-remote -h $(IBROWSE_URI) master 2>/dev/null` ;	\
	git_ok=$$? ; 								\
	set -e ; 								\
	if [ -f $(IBROWSE_VSN) -a $$git_ok -eq 0 ] ; then 			\
	  nvsn=`echo $$nvsn | awk '{print $$1}'` ; 				\
	  vsn=`cat $(IBROWSE_VSN)` ; 						\
	  [ "$$nvsn" = "$$vsn" ] || fetch=yes ; 				\
	else 									\
	  if [ ! -f $(IBROWSE_VSN) -a $$git_ok -ne 0 ] ; then 			\
	    echo "error: ibrowse repository $(IBROWSE_URI) unreachable" ; 	\
	    exit 1 ; 								\
	  fi ; 									\
	  [ $$git_ok -eq 0 ] && fetch=yes ; 					\
	fi ; 									\
	if [ -n "$$fetch" ] ; then 						\
	  rm -rf ibrowse ; 							\
	  echo "  FETCH ibrowse" ;						\
	  curl -s -L -o $(IBROWSE_TGZ) $(IBROWSE_DOWNLOAD_URI) ;		\
	  dir=`tar ztf $(IBROWSE_TGZ) | head -1 | sed -e 's/\/$$//'` ; 		\
	  tar zxf $(IBROWSE_TGZ) ;						\
	  mv $$dir $(top_builddir)/test/ibrowse ;				\
	  rm -f $(IBROWSE_TGZ) ; 						\
	  echo $$nvsn > $(IBROWSE_VSN) ; 					\
	fi

#####
# Overridable targets (to override a target, use '::' separator)
# Note: using '::', all targets with the same name will be executed
####
prepare-test:: clean-test
	$(AM_V_at)$(INSTALL) -d docroot-test
	$(AM_V_at)$(INSTALL) -d logs

clean-test::
	$(AM_V_at)rm -fr logs docroot-test erl_crash.dump report.log *.access *.auth

.PHONY: ibrowse
.NOTPARALLEL:


# Local Variables:
#    tab-width: 8
# End:
