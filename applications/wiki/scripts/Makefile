#    -*- Makefile -*- 
#    File:	 Makefile  (/usr/local/src/yaws/yaws/applications/wiki/scripts/Makefile)
#    Author:	 Johan Bevemyr
#    Created:	 Sat May 17 13:15:29 2003
#    Purpose:   
 
include ../include.mk

SCRIPTS=importFiles getPassword updateWiki addFile

all: $(SCRIPTS)

addFile : addFile.src
	sed "s|%%ebindir%%|$(EBININSTALLDIR)|" $< > $@
	chmod +x $@

importFiles : importFiles.src
	sed "s|%%ebindir%%|$(EBININSTALLDIR)|" $< > $@
	chmod +x $@

getPassword : getPassword.src
	sed "s|%%ebindir%%|$(EBININSTALLDIR)|" $< > $@
	chmod +x $@

updateWiki : updateWiki.src
	sed "s|%%wikisrcdir%%|$(shell cd ../wiki ; pwd)|" $< > $@
	chmod +x $@

clean:
	rm importFiles getPassword updateWiki


install:


