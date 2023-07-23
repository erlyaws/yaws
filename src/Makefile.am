include @top_srcdir@/include.mk

STATIC_MODULES = yaws.erl			\
		 yaws_app.erl			\
		 yaws_ticker.erl		\
		 yaws_config.erl		\
		 yaws_server.erl		\
		 yaws_sup.erl			\
		 yaws_api.erl			\
		 yaws_log.erl			\
		 yaws_trace.erl			\
		 yaws_logger.erl		\
		 yaws_ls.erl			\
		 yaws_debug.erl			\
		 yaws_compile.erl		\
		 yaws_ctl.erl			\
		 yaws_cgi.erl			\
		 yaws_zlib.erl			\
		 mime_type_c.erl		\
		 yaws_session_server.erl	\
		 yaws_outmod.erl		\
		 yaws_revproxy.erl		\
		 yaws_html.erl			\
		 yaws_rss.erl			\
		 yaws_appmod_dav.erl		\
		 yaws_runmod_lock.erl		\
		 yaws_pam.erl			\
		 json2.erl			\
		 jsonrpc.erl			\
		 yaws_jsonrpc.erl		\
		 yaws_xmlrpc.erl		\
		 haxe.erl			\
		 yaws_rpc.erl			\
		 yaws_soap_srv.erl		\
		 yaws_soap_lib.erl		\
		 yaws_soap12_lib.erl		\
		 yaws_appmod_cgi.erl		\
		 yaws_appmod_fcgi.erl		\
		 yaws_sendfile.erl		\
		 yaws_sup_restarts.erl		\
		 yaws_stats.erl			\
		 yaws_vdir.erl			\
		 yaws_multipart.erl		\
		 yaws_shaper.erl		\
		 yaws_dime.erl			\
		 yaws_exhtml.erl		\
		 yaws_sse.erl			\
		 yaws_websockets.erl		\
		 yaws_dynopts.erl		\
		 yaws_ws_sup.erl

EXTRA_DIST = $(STATIC_MODULES) yaws_websockets.erl yaws_debug.hrl	\
	     yaws_generated.template yaws.app.src yaws.app.src.script	\
	     yaws.appup.src

GENERATED_MODULES = yaws_generated.erl mime_types.erl
MODULES           = $(STATIC_MODULES) $(GENERATED_MODULES)


EBIN_DIR=@top_builddir@/ebin
EBIN_FILES=$(MODULES:%.erl=$(EBIN_DIR)/%.beam)

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS)

include @top_srcdir@/erlang_deps.mk

all-local: $(EBIN_DIR)/yaws.app $(EBIN_DIR)/yaws.appup $(EBIN_FILES)

tags: $(EBIN_DIR)/yaws_generated.beam $(EBIN_DIR)/yaws_debug.beam
	$(AM_V_GEN)erl  -noshell -pa $(EBIN_DIR) -s yaws_debug mktags

xref: $(EBIN_DIR)/yaws_generated.beam $(EBIN_DIR)/yaws_debug.beam
	$(AM_V_GEN)erl -noshell -pa $(EBIN_DIR) -s yaws_debug xref $(EBIN_DIR)

clean-local:
	$(AM_V_at)rm -f $(EBIN_DIR)/yaws.app $(EBIN_DIR)/yaws.appup $(EBIN_FILES)
	$(AM_V_at)rm -f yaws_generated.erl mime_types.erl


yaws_generated.erl: @srcdir@/yaws_generated.template @top_srcdir@/vsn.mk \
		    $(EBIN_DIR)/yaws.beam
	$(AM_V_at)(cd @srcdir@ &&	\
	  YAWS_VSN='@YAWS_VSN@'		\
	  VARDIR='@localstatedir@'	\
	  ETCDIR='@sysconfdir@'		\
	    @abs_top_srcdir@/scripts/gen-yaws-generated) > yaws_generated.erl


mime_types.erl: @top_builddir@/priv/mime.types $(EBIN_DIR)/mime_type_c.beam \
		$(EBIN_DIR)/yaws.beam
	$(AM_V_GEN)$(ERL) -noshell -pa $(EBIN_DIR) -s mime_type_c generate

$(EBIN_DIR)/yaws.app: @srcdir@/yaws.app.src
	$(AM_V_GEN)$(AWK) -v src="$(MODULES)"				\
	"/{vsn, *{cmd,/ { 						\
		line = \$$0; 						\
		sub(/{vsn.*/, \"{vsn, \\\"@YAWS_VSN@\\\"},\", line);	\
		print line; 						\
		next; 							\
	}								\
	/{modules,/ {							\
		line = \$$0;						\
		indent = index(line, \"[\"); 				\
		line = substr(line, 1, indent);				\
		print line;						\
		split(src, mods);					\
		mods_count = 0; 					\
		for (mod in mods) { 					\
			mods_count++; 					\
		} 							\
		prefix = sprintf(\"%*s\", indent, \" \");		\
		for (i = 1; i <= mods_count; i++) { 			\
			mod = mods[i]; 					\
			sub(/\.erl/, \"\", mod); 			\
			if (i != mods_count) 				\
				print prefix mod \",\";			\
			else 						\
				print prefix mod;			\
		}							\
		print prefix \"]},\";					\
		next;							\
	} 								\
	/\@APPDEPS\@/ {							\
		line = \$$0;						\
		sub(/\@APPDEPS\@/, \"$(APPDEPS)\", line);		\
		print line; 						\
		next;							\
	}								\
	{ print; }" $< > $@

$(EBIN_DIR)/yaws.appup: @srcdir@/yaws.appup.src
	$(AM_V_GEN)sed -e "s/%VSN%/@YAWS_VSN@/g" < $< > $@

$(EBIN_DIR)/%.beam: %.erl
	$(AM_V_at)$(ERLC) $(ERLC_FLAGS) -M $(PHONY_DEPS) -MF $(@:$(EBIN_DIR)/%.beam=$(DEPDIR)/%.Pbeam) -MT $@ $<
	$(AM_V_ERLC)$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<


distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
