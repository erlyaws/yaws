include ../include.mk


# don't c-compile anything on win32 (yet)
# I don't know how to make linked in driver using gcc

ifdef WIN32
	PRIV_FILES=../priv/.foo
else
	PRIV_FILES= ../priv/setuid_drv.$(DLL)  ../priv/epam
endif


CFLAGS += -I"${ERLDIR}/usr/include"

#
# Targets
#

all: $(PRIV_FILES) 

clean:
	-rm -f $(PRIV_FILES) setuid_drv.$(OBJ) 

install:
	install -d $(INSTALLPREFIX)/lib/yaws/priv
	install $(PRIV_FILES) $(INSTALLPREFIX)/lib/yaws/priv
ifndef WIN32
	chown root $(INSTALLPREFIX)/lib/yaws/priv/epam
	chmod a+s $(INSTALLPREFIX)/lib/yaws/priv/epam
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
