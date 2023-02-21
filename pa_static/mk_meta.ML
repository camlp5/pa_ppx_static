#include "version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|
# Specifications for the "pa_ppx_static" preprocessor:
requires = "camlp5,fmt,pa_ppx.base,pa_ppx_static_runtime,camlp5.parser_quotations"
version = "%s"
description = "pa_ppx_static: pa_ppx_static rewriter"

# For linking
package "link" (
requires = "camlp5,fmt,pa_ppx.base.link,camlp5.parser_quotations.link"
archive(byte) = "pa_ppx_static.cmo"
archive(native) = "pa_ppx_static.cmx"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_static.cmo"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx.base,camlp5.parser_quotations"
  archive(syntax,preprocessor,-native) = "pa_ppx_static.cmo"
  archive(syntax,preprocessor,native) = "pa_ppx_static.cmx"

|} version)
