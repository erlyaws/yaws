include ../include.mk


all debug:	../bin/yaws yaws.conf

../bin/yaws:	yaws.template
	cat yaws.template | \
		./Subst %yawsdir% `pwd`/..  | \
		./Subst %vardir% $(VARDIR) | \
		./Subst %run_erl% "$(ERLBINDIR)/run_erl" | \
		./Subst %to_erl% "$(ERLBINDIR)/to_erl" | \
		./Subst %erl% "$(ERL)" | \
		./Subst %werl% "$(WERL)" > ../bin/yaws
	chmod +x ../bin/yaws


local_install:	../bin/yaws
	@install -d $$HOME/yaws_logs
	@if [ -f $$HOME/yaws.conf ]; \
	    then target=$$HOME/yaws.conf.template; \
        else \
	    target=$$HOME/yaws.conf; \
	fi; \
	echo "--- Installing local config file at $${target} "; \
	cat yaws.conf.template | \
		./Subst %yawsdir% `pwd`/.. | \
		./Subst %logdir% $$HOME/yaws_logs | \
		./Subst %host% `hostname` | \
		./Subst %port% 8000 | \
		./Subst %vardir% $(VARDIR) | \
		./Subst %docroot% `pwd`/../www | \
		./Subst %certdir% `pwd`/../ssl | \
		./Subst %sslport% 4443 > $${target}; \
	$(INSTALL) -d $$HOME/bin; \
	rm $$HOME/bin/yaws 2> /dev/null; \
	cp -f ../bin/yaws $$HOME/bin/yaws; \
	echo "--- Installed  $$HOME/bin/yaws"


yaws.conf:
	@echo PREFIX is $(PREFIX)
	cat yaws.conf.template | \
		./Subst %yawsdir% $(PREFIX)/lib/yaws | \
		./Subst %logdir% $(VARDIR)/log/yaws | \
		./Subst %vardir% $(VARDIR) | \
		./Subst %host% `hostname` | \
		./Subst %port% 80 | \
		./Subst %docroot% $(VARDIR)/yaws/www | \
		./Subst %certdir% $(ETCDIR) | \
		./Subst %sslport% 443 > yaws.conf

clean:
	rm -f ../bin/yaws yaws.conf

install:	.install

.install:
	-rm -rf /tmp/yaws 2> /dev/null
	sh  ./Install $(PREFIX) $(ERL) \
		$(WERL) $(ETCDIR)/ $(VARDIR)/ $(ERLBINDIR)/ $(DESTDIR)
	@cp ../ssl/yaws-cert.pem $(DESTDIR)$(ETCDIR)
	@cp ../ssl/yaws-key.pem $(DESTDIR)$(ETCDIR)
	@if [ -f $(DESTDIR)$(ETCDIR)/yaws.conf ]; \
		then echo "Keeping old config file "; \
		cp yaws.conf $(DESTDIR)$(ETCDIR)/yaws.conf.template; \
	else \
		echo "Installing $(DESTDIR)$(ETCDIR)/yaws.conf"; \
		mkdir -p $(DESTDIR)$(ETCDIR); \
		cp yaws.conf $(DESTDIR)$(ETCDIR)/yaws.conf; \
	fi
