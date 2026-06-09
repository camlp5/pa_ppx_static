WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub
include $(TOP)/config/Makefile.mdx

all test:: mdx-test

mdx-test:: toplevel_test.asciidoc.TEST ../README.asciidoc.TEST toplevel-error-handling.asciidoc.TEST
