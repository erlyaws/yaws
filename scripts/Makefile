include ../include.mk


all debug:	yaws.ss
	@sh ./mangle `pwd`/.. $(ERL) yaws.ss ../bin/yaws
	@chmod +x ../bin/yaws
	@if [ -f $$HOME/yaws.conf ]; \
           then echo "$$HOME/yaws.conf already exists ";  \
        else \
	   sh ./mangle `pwd`/.. $(ERL) yaws.conf.template.local ${HOME}/yaws.conf; echo "installed yaws.conf in $$HOME "; fi


clean:
	rm -f ../bin/yaws

install:	all
	sh ./Install $(INSTALLPREFIX) $(ERL)




