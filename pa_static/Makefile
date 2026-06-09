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

all: $(CMO) $(CMX) META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META mk_meta mk_meta.ml
