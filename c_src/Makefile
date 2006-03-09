include ../include.mk


# don't c-compile anything on win32 (yet)
# I don't know how to make linked in driver using gcc

ifdef WIN32
	PRIV_FILES=../priv/.foo
else
	PRIV_FILES= ../priv/setuid_drv.$(DLL)  ../priv/epam
endif


CFLAGS += -I"${ERLDIR}/usr/include"

# and for MacOs + possibly others ...
CFLAGS += -I/usr/include/pam/

#
# Targets
#

all: $(PRIV_FILES) 

clean:
	-rm -f $(PRIV_FILES) setuid_drv.$(OBJ) 

install:
	$(INSTALL) -d $(INSTALLPREFIX)/lib/yaws/priv
	$(INSTALL) $(PRIV_FILES) $(INSTALLPREFIX)/lib/yaws/priv
ifndef WIN32
	if [ `id -u` = "0" ]; then \
		chown root $(INSTALLPREFIX)/lib/yaws/priv/epam; \
		chmod a+s $(INSTALLPREFIX)/lib/yaws/priv/epam; \
	fi
endif


../priv/setuid_drv.$(DLL): setuid_drv.$(OBJ)
	$(LD_SHARED) $(OUT) $@  setuid_drv.$(OBJ) $(DLL_LIBS)
setuid_drv.$(OBJ):	setuid_drv.c
	$(CC) -c $(FPIC) $(CFLAGS) -DDYNAMIC_DRIVER setuid_drv.c

../priv/.foo:
	touch ../priv/.foo	

epam.o:	epam.c
	$(CC) -c $(CFLAGS) epam.c
../priv/epam: epam.o
	$(CC) -o $@ epam.o -lpam
