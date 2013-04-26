LIB_DIR=$(CURDIR)/rel/batnitor/lib
BATNITOR_LIB_DIR=$(shell echo $(LIB_DIR)/batnitor-*)

.PHONY: all debug clean

all: deps
	./rebar compile generate

all_release: deps
	./rebar -C rebar.config.release compile generate

deps:
	./rebar get-deps

debug:
	rm -rf $(BATNITOR_LIB_DIR)/ebin
	ln -s ../../../../ebin $(BATNITOR_LIB_DIR)/ebin
	./rel/batnitor/bin/batnitor console_boot batnitor_debug

clean:
	./rebar clean

