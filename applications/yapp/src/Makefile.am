include @top_srcdir@/include.mk

MODULES = yapp_app.erl		\
	  yapp.erl		\
	  yapp_ets_server.erl	\
	  yapp_event_handler.erl\
	  yapp_handler.erl	\
	  yapp_mnesia_server.erl\
	  yapp_registry.erl	\
	  yapp_server.erl	\
	  yapp_sup.erl

EXTRA_DIST = $(MODULES) yapp.app.src yapp.appup.src

EBIN_DIR=@builddir@/../ebin
EBIN_FILES=$(MODULES:%.erl=$(EBIN_DIR)/%.beam)

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS)

include @top_srcdir@/erlang_deps.mk


all-local: $(EBIN_DIR)/yapp.app $(EBIN_DIR)/yapp.appup $(EBIN_FILES)

clean-local:
	$(AM_V_at)rm -f $(EBIN_DIR)/yapp.app $(EBIN_DIR)/yapp.appup $(EBIN_FILES)

$(EBIN_DIR)/yapp.app: @srcdir@/yapp.app.src
	$(AM_V_GEN)$(AWK) -v src="$(MODULES)"				\
	"/{vsn, *{cmd,/ { 						\
		line = \$$0; 						\
		sub(/{vsn.*/, \"{vsn, \\\"@YAPP_VSN@\\\"},\", line);	\
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
	{ print; }" $< > $@

$(EBIN_DIR)/yapp.appup: @srcdir@/yapp.appup.src
	$(AM_V_GEN)sed -e "s/%VSN%/@YAPP_VSN@/g" < $< > $@

$(EBIN_DIR)/%.beam: %.erl
	$(AM_V_at)$(ERLC) $(ERLC_FLAGS) -M $(PHONY_DEPS) -MF $(@:$(EBIN_DIR)/%.beam=$(DEPDIR)/%.Pbeam) -MT $@ $<
	$(AM_V_ERLC)$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<


distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
