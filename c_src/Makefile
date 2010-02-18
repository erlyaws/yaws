include ../include.mk


# don't c-compile anything on win32 (yet)
# I don't know how to make a linked in driver using gcc, or any
# other compiler there, and have no intentions of finding out

ifdef WIN32
	PRIV_FILES=../priv/.foo
else
# don't make pam if configured out with --disable-pam
	PRIV_FILES= ../priv/lib/setuid_drv.$(DLL)  $(EPAM)
endif

ifeq ($(HAVE_SENDFILE),true)
	PRIV_FILES += ../priv/lib/yaws_sendfile_drv.$(DLL)
endif


CFLAGS += -I"${ERLDIR}/usr/include"

# and for MacOs + possibly others ...
CFLAGS += -I/usr/include/pam/ $(EXTRAINCLUDE)

#
# Targets
#

all: $(PRIV_FILES) 
debug: 
	CFLAGS="$(CFLAGS) -g" $(MAKE) all

clean:
	-rm -f $(PRIV_FILES) setuid_drv.$(OBJ) epam.$(OBJ) yaws_sendfile_drv.$(OBJ) hashtable.$(OBJ)

install:	$(PRIV_FILES)
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/lib/yaws/priv/lib
	$(INSTALL) $(PRIV_FILES) $(DESTDIR)$(PREFIX)/lib/yaws/priv/lib
ifndef WIN32
	if [ `id -u` = "0" -a -d $(DESTDIR)$(PREFIX)/lib/yaws/priv/epam ]; then \
		chown root $(DESTDIR)$(PREFIX)/lib/yaws/priv/epam; \
		chmod a+s $(DESTDIR)$(PREFIX)/lib/yaws/priv/epam; \
	fi
endif


../priv/lib/setuid_drv.$(DLL): setuid_drv.$(OBJ)
	$(LD_SHARED) $(OUT) $@ $< $(DLL_LIBS)
setuid_drv.$(OBJ):	setuid_drv.c
	$(CC) -c $(FPIC) $(CFLAGS) -DDYNAMIC_DRIVER $<

../priv/lib/yaws_sendfile_drv.$(DLL): yaws_sendfile_drv.$(OBJ) hashtable.$(OBJ)
	$(LD_SHARED) $(OUT) $@ $^ $(DLL_LIBS)
yaws_sendfile_drv.$(OBJ):	yaws_sendfile_drv.c
	$(CC) -c $(FPIC) $(CFLAGS) -DDYNAMIC_DRIVER $<
hashtable.$(OBJ):	hashtable.c
	$(CC) -c $(FPIC) $(CFLAGS) $<

../priv/.foo:
	touch $@

epam.$(OBJ):	epam.c
	$(CC) -c $(CFLAGS) $<
../priv/epam: epam.$(OBJ)
	$(CC) -o $@ $(CFLAGS) $< -lpam
