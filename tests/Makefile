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

## NOTE WELL:  toplevel_test.asciidoc.TEST is not being tested (b/c MDX is backlevel)
test:: all
	mkdir -p _build && ./pa_ppx_static_test.exe
#	$(MAKE) toplevel_test.asciidoc.TEST

toplevel_test.asciidoc.corrected: use_test_ml

toplevel::
	$(LAUNCH) ocaml -nopromptcont

pa_ppx_static_test.exe: pa_ppx_static_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

