# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile

WD=$(shell pwd)
DESTDIR=
RM=rm

SYSDIRS= runtime pa_typedstatic

TESTDIRS= tests

all: sys
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

sys:
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

test: all
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

mdx-test:: README.asciidoc.TEST

.PRECIOUS: %.asciidoc.corrected

%.asciidoc.corrected: %.asciidoc
	$(LAUNCH) ocaml-mdx test -o $@ $^

%.asciidoc.TEST: %.asciidoc.corrected %.asciidoc
	diff -Bwiu $^

META: all
	$(JOINMETA) -rewrite pa_ppx_typestatic_runtime:pa_ppx_typedstatic.runtime \
			-direct-include pa_typedstatic \
			-wrap-subdir runtime:runtime > META

install: META
	$(OCAMLFIND) remove pa_ppx_typedstatic || true
	$(OCAMLFIND) install pa_ppx_typedstatic META local-install/lib/*/*.*

uninstall:
	$(OCAMLFIND) remove pa_ppx_typedstatic || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done
	rm -rf docs local-install $(BATCHTOP) META *.corrected

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
