beamdir = $(ERLANG_INSTALL_LIB_DIR_yaws)/ebin
EBIN_DIR=@top_builddir@/ebin

APP_FILES = $(EBIN_DIR)/yaws.app $(EBIN_DIR)/yaws.appup
BEAM_FILES = $(EBIN_DIR)/*.beam

beam_DATA = $(APP_FILES) $(BEAM_FILES)


clean-local:
	$(AM_V_at)rm -f $(APP_FILES) $(BEAM_FILES)

# Local Variables:
#    tab-width: 8
# End:
