include ../include.mk


all debug:	../bin/yaws yaws.conf

../bin/yaws:	yaws.template
	cat yaws.template | \
		./Subst %yawsdir% `pwd`/..  | \
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
		./Subst %docroot% `pwd`/../www | \
		./Subst %certdir% `pwd`/../ssl | \
		./Subst %sslport% 4443 > $${target}; \
	install -d $$HOME/bin; \
	cp ../bin/yaws $$HOME/bin/yaws; \
	echo "--- Installed  $$HOME/bin/yaws"

yaws.conf:
	echo PREFIX is $(PREFIX)
	cat yaws.conf.template | ./Subst %yawsdir% $(PREFIX)/lib/yaws | ./Subst %logdir% /var/log/yaws | ./Subst %host% `hostname` | ./Subst %port% 80 | ./Subst %docroot% $(VARDIR)/yaws/www | ./Subst %certdir% $(ETCDIR) | ./Subst %sslport% 443 > yaws.conf

clean:
	rm -f ../bin/yaws

install:
	-rm -rf /tmp/yaws 2> /dev/null
	sh ./Install $(PREFIX) $(INSTALLPREFIX) "$(ERL)" \
		"$(WERL)" /etc/ /var/
	@cp ../ssl/yaws-cert.pem $(ETCDIR)
	@cp ../ssl/yaws-key.pem $(ETCDIR)
	@if [ -f $(prefix)/etc/yaws.conf ]; \
		then echo "Keeping old config file "; \
		cp yaws.conf $(prefix)/etc/yaws.conf.template; \
	else \
		echo "Installing $(prefix)/etc/yaws.conf"; \
		cp yaws.conf $(prefix)/etc/yaws.conf; \
	fi
