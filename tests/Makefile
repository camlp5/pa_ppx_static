WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

SYNTAX := camlp5o

PACKAGES := $(PACKAGES),pa_ppx_static,ounit2,pa_ppx.utils

ML= \
	pa_ppx_static_test.ml

CMO := $(ML:.ml=.cmo)

TESTS=pa_ppx_static_test.exe

all: $(TESTS)

test:: all
	mkdir -p _build && ./pa_ppx_static_test.exe

toplevel::
	$(LAUNCH) ocaml -nopromptcont

pa_ppx_static_test.exe: pa_ppx_static_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

EXTERNAL := $(shell $(LAUNCH) $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(TESTS): $(EXTERNAL)

depend::
	echo "$(CMO) $(TESTS): $(EXTERNAL)" > .depend.NEW
	$(LAUNCH) $(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o \
	$(ML) \
	>> .depend.NEW
	mv .depend.NEW .depend

-include .depend
