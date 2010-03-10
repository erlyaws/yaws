
YAWS := $(wildcard *.yaws)
CHECK := $(YAWS:%.yaws=%.check)

all:	$(CHECK)

clean:
	rm *.check

%.check:	%.yaws
	../../bin/yaws --check $< ../include && touch $@
