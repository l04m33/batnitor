.PHONY: all debug clean

all:
	./rebar compile generate

debug:
	./rel/batnitor/bin/batnitor console_boot batnitor_debug

clean:
	./rebar clean

