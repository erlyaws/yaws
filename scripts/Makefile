include ../include.mk


all:	yaws.ss
	sh ./mangle `pwd`/../ebin $(ERL) yaws.ss ../bin/yaws

clean:
	rm -f ../bin/yaws

install:	all
	sh ./mangle $(INSTALLPREFIX)/lib/yaws $(ERL)  \
		yaws.ss $(INSTALLPREFIX)/bin/yaws; \
	chmod +x $(INSTALLPREFIX)/bin/yaws; \
	[ -f /etc/yaws.conf ] && mv -f /etc/yaws.conf /etc/yaws.conf.bak; \
	sh ./mangle $(INSTALLPREFIX)/lib/yaws foo \
		yaws.conf.template /etc/yaws.conf


