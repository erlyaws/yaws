include ../support/include.mk

all:	conf setup app_test.beam
	echo "all ok"

# invoke as 
# TEST=test3 make test
# or just make test to run all tests
test:	all
	dd if=/dev/zero of=../../www/1000.txt count=1000
	dd if=/dev/zero of=../../www/2000.txt count=2000
	dd if=/dev/zero of=../../www/3000.txt count=2000
	$(ERL) -noinput $(PA) -s tftest
	$(MAKE) stop


debug:
	$(ERL) $(PA) 

conf:	stdconf

clean:	tclean
	-rm -rf localhost:8000




