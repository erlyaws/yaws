include ../include.mk


all:	yaws.ss
	sh ./mangle `pwd`/../ebin $(ERL) ../bin/yaws

clean:
	rm -f ../bin/yaws

install:	all
	sh ./mangle $(INSTALLPREFIX)/lib/yaws/ebin $(ERL)  \
		$(INSTALLPREFIX)/bin/yaws
	[ -f /etc/yaws.conf ] && mv -f /etc/yaws.conf /etc/yaws.conf.bak
	$(INSTALL_DATA) yaws.conf.template /etc/yaws.conf
