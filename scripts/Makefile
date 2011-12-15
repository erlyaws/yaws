include ../include.mk


all debug:	../bin/yaws yaws.conf

../bin/yaws:	yaws.template
	VARDIR='$(VARDIR)' ERLBINDIR='$(ERLBINDIR)' \
	ERL='$(ERL)' WERL='$(WERL)' ./gen-yaws > $@
	chmod +x $@


local_install:	../bin/yaws
	VARDIR="${HOME}/var" ./local-install true

yaws.conf:
	YAWSDIR='$(PREFIX)/lib/yaws' LOGDIR='$(PREFIX)/var/log/yaws' \
	VARDIR='$(VARDIR)' PORT=80 DOCROOT='$(VARDIR)/yaws/www' \
	CERTDIR='$(ETCDIR)/yaws' SSLPORT=443 ./gen-yaws-conf > $@

clean:
	rm -f ../bin/yaws yaws.conf

install:	.install

.install:
	PREFIX='$(PREFIX)' ETCDIR='$(ETCDIR)' VARDIR='$(VARDIR)' \
	ERLBINDIR='$(ERLBINDIR)' DESTDIR='$(DESTDIR)' \
	ERL='$(subst ",,$(ERL))' WERL='$(subst ",,$(WERL))' ./regular-install
