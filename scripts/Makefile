include ../include.mk


all debug:	../bin/yaws

../bin/yaws:	yaws.ss
	@sh ./mangle `pwd`/.. $(ERL) yaws.ss ../bin/yaws
	@chmod +x ../bin/yaws

local_install:	../bin/yaws
	@if [ -f $$HOME/yaws.conf ]; \
           then echo "$$HOME/yaws.conf already exists ";  \
        else \
	   sh ./mangle `pwd`/.. $(ERL) yaws.conf.template.local ${HOME}/yaws.conf; echo "installed yaws.conf in $$HOME "; fi
	install -d $$HOME/bin
	cp ../bin/yaws $$HOME/bin/yaws


clean:
	rm -f ../bin/yaws

install:	all
	sh ./Install $(INSTALLPREFIX) $(ERL)




