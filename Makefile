.PHONY: all debug clean

all:
	./rebar compile generate

debug:
	./rel/batnitor/bin/batnitor console

clean:
	./rebar clean

