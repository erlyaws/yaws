include ../include.mk


all debug:	../bin/yaws

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

clean:
	rm -f ../bin/yaws

install:
	-rm -rf /tmp/yaws 2> /dev/null
	sh ./Install $(PREFIX) $(INSTALLPREFIX) "$(ERL)" \
		"$(WERL)" $(ETCDIR) $(VARDIR)
	@cp ../ssl/yaws-cert.pem $(ETCDIR)
	@cp ../ssl/yaws-key.pem $(ETCDIR)




