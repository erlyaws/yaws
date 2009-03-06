
include ../include.mk

install:
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -d $(DESTDIR)$(PREFIX)/share/man/man5
	cp yaws.1 $(DESTDIR)$(PREFIX)/share/man/man1
	cp yaws.conf.5 $(DESTDIR)$(PREFIX)/share/man/man5
	cp yaws_api.5 $(DESTDIR)$(PREFIX)/share/man/man5



## for the win32 build, the rest of us know how to run man
pdf:
	man -t ./yaws.1 > xxx;  ps2pdf xxx yaws.1.pdf
	man -t ./yaws.conf.5 > xxx; ps2pdf xxx yaws.conf.5.pdf
	man -t ./yaws_api.5 > xxx; ps2pdf xxx yaws_api.5.pdf
	man -t ./yaws_soap_lib.5 > xxx; ps2pdf xxx yaws_soap_lib.5.pdf
	rm xxx;

clean:
	rm *.pdf 2>/dev/null || true


all debug:
