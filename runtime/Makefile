# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

PACKAGES := fmt,camlp5,pcre,pa_ppx.base
TARGET := pa_ppx_typedstatic_runtime.cma
ML := runtime.ml
TOOLSML = mk_meta.ml
MLI := 
CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_typedstatic

all: $(TARGET) $(TARGET:.cma=.cmxa) META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

pa_ppx_typedstatic.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_typedstatic.cmo $^

pa_ppx_typedstatic.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_typedstatic.cmx $^

META: mk_meta
	./mk_meta > META

mk_meta: mk_meta.cmo
	$(OCAMLFIND) ocamlc -linkpkg -linkall -package $(PACKAGES) $< -o $@

mk_meta.ml: mk_meta.ML
	cppo $< > $@

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_typedstatic_runtime -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) pa_ppx_typedstatic.cmx pa_ppx_typedstatic.cmi $(wildcard *.cmt*)

clean::
	rm -rf META mk_meta mk_meta.ml

$(TARGET): pa_ppx_typedstatic.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) pa_ppx_typedstatic.cmo -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_typedstatic.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) pa_ppx_typedstatic.cmx -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep -package $(PACKAGES) -syntax camlp5o runtime.ml >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
