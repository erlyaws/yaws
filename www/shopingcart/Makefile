
all:	shopcart.beam

debug:

install:	all
	cp shopcart.beam /var/yaws/ebin

clean:
	$(RM) shopcart.beam

shopcart.beam:	shopcart.erl
	erlc shopcart.erl
