include ../support/include.mk


# 
all:	conf setup app_test.beam
	@echo "all ok"



# invoke as 
# TEST=test3 make test
# or just make test to run all


test:	conf start
	$(ERL) -noinput $(PA) -s tftest
	$(MAKE) stop


debug:
	$(ERL) $(PA) 

conf:	authconf

clean:	tclean
	-rm -rf localhost:8000 logs yaws.conf
