include ../include.mk


all:	yaws.ss
	sh ./mangle `pwd`/.. $(ERL) yaws.ss ../bin/yaws
	chmod +x ../bin/yaws
	sh ./mangle `pwd`/.. $(ERL) yaws.conf.template.local ${HOME}/yaws.conf

clean:
	rm -f ../bin/yaws

install:	all
	sh ./mangle $(INSTALLPREFIX)/lib/yaws $(ERL)  \
		yaws.ss $(INSTALLPREFIX)/bin/yaws; \
	chmod +x $(INSTALLPREFIX)/bin/yaws; \
	[ -f /etc/yaws.conf ] && mv -f /etc/yaws.conf /etc/yaws.conf.bak; \
	sh ./mangle $(INSTALLPREFIX)/lib/yaws foo \
		yaws.conf.template /etc/yaws.conf


