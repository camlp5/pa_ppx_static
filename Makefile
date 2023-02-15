# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile.top

WD=$(shell pwd)
DESTDIR=

SYSDIRS= runtime pa_static

TESTDIRS= tests

all: sys
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

sys:
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

test: all
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

mdx-test:: README.asciidoc.TEST

META: sys
	$(JOINMETA) -rewrite pa_ppx_static_runtime:pa_ppx_static.runtime \
			-direct-include pa_static \
			-wrap-subdir runtime:runtime > META

install: META
	$(OCAMLFIND) remove pa_ppx_static || true
	$(OCAMLFIND) install pa_ppx_static META local-install/lib/*/*.*

uninstall:
	$(OCAMLFIND) remove pa_ppx_static || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done
	rm -rf docs local-install $(BATCHTOP) META *.corrected

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
