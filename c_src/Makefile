include ../include.mk


PRIV_FILES= ../priv/setuid_drv.$(DLL) 

CFLAGS += -I${ERLDIR}/usr/include

#
# Targets
#

all: $(PRIV_FILES) 

clean:
	-rm -f $(PRIV_FILES) setuid_drv.$(OBJ) 

install:
	install -d $(INSTALLPREFIX)/lib/yaws/priv
	install ../priv/setuid_drv.$(DLL) $(INSTALLPREFIX)/lib/yaws/priv


../priv/setuid_drv.$(DLL): setuid_drv.$(OBJ)
	$(LD_SHARED) $(OUT)$@ setuid_drv.$(OBJ) $(DLL_LIBS)

setuid_drv.$(OBJ):	setuid_drv.c
	$(CC) -c $(FPIC) $(CFLAGS) -DDYNAMIC_DRIVER setuid_drv.c





