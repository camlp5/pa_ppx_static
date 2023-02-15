WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),pa_ppx_static,ounit2,pa_ppx.utils

ML= \
	pa_ppx_static_test.ml

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=pa_ppx_static_test.exe

all: $(TESTS)


test:: all toplevel_test.asciidoc.TEST
	mkdir -p _build && ./pa_ppx_static_test.exe

toplevel_test.asciidoc.corrected: use_test_ml

toplevel::
	$(LAUNCH) ocaml -nopromptcont

pa_ppx_static_test.exe: pa_ppx_static_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

