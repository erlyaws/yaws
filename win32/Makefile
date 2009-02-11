##
## Debian Linux mingw cross-compiler version:
## To install 
# sudo apt-get install mingw32


CC	= i586-mingw32msvc-gcc
AR	= i586-mingw32msvc-ar
LD	= i586-mingw32msvc-gcc


CP	= cp
RM	= rm -f
#AR	= i586-mingw32msvc-ar
MKDIR	= -mkdir

##
## Definitions:
##
CFLAGS = -O2 -Wall
INCLUDES = -I.


## 
## Global Makefile settings
##
OBJS = \
	yaws.o 

.c.o:
	$(CC) -c $(CFLAGS) $(INCLUDES) -o $@ $<

##
## Targets
##
all:	yaws.exe
	./mkinstaller.sh


yaws.exe:	$(OBJS) 
	$(CC) $(CFLAGS) $(OBJS) -o yaws.exe


clean:
	$(RM) *.o
	$(RM) *.a
	$(RM) *.exe
	$(RM) -rf ebin examples applications ssl src www include doc priv bin logs scripts vsn.mk  LICENSE.txt 

