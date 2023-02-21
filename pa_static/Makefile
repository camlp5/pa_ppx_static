# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),fmt,pa_ppx.base,pa_ppx.deriving_plugins.std
TARGET_PACKAGE = pa_ppx_static

ML = pa_ppx_static.ml
TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

export MIGRATE_CFLAGS= -ppopt -pa_import-I -ppopt . -ppopt -pa_passthru-debug \
	-ppopt -pa_import-package -ppopt $(PACKAGES)
export MIGRATE_PACKAGES=pa_ppx.import,pa_ppx_migrate

all: $(CMO) camlp5.pa_ppx_static camlp5.pa_ppx_static.opt META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

camlp5.pa_ppx_static: $(CMO)
	$(MKCAMLP5) -package fmt,re,camlp5.pa_r,camlp5.pr_r,pa_ppx.base $(CMO) -o $@

camlp5.pa_ppx_static.opt: $(CMX)
	$(MKCAMLP5OPT) -package fmt,re,camlp5.pa_r,camlp5.pr_r,pa_ppx.base $(CMX) -o $@

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META camlp5.pa_ppx_static* mk_meta mk_meta.ml
