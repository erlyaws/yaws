#    -*- Makefile -*- 
#    File:	 Makefile  (/usr/local/src/yaws/yaws/applications/wiki/scripts/Makefile)
#    Author:	 Johan Bevemyr
#    Created:	 Sat May 17 13:15:29 2003
#    Purpose:   
 
include ../include.mk

SCRIPTS=importFiles getPassword

all: $(SCRIPTS)

importFiles : importFiles.src
	sed "s|%%ebindir%%|$(EBININSTALLDIR)|" $< > $@
	chmod +x $@

getPassword : getPassword.src
	sed "s|%%ebindir%%|$(EBININSTALLDIR)|" $< > $@
	chmod +x $@

clean:
	rm importFiles getPassword
