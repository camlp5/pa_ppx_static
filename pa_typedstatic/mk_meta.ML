#include "../version.inc"

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
# Specifications for the "pa_ppx_typedstatic" preprocessor:
requires = "camlp5,fmt,pa_ppx.base,pa_ppx_typedstatic_runtime,camlp5.parser_quotations"
version = "%s"
description = "pa_ppx_typedstatic: pa_ppx_typedstatic rewriter"

# For linking
package "link" (
requires = "camlp5,fmt,pa_ppx.base.link,camlp5.parser_quotations.link"
archive(byte) = "pa_ppx_typedstatic.cma"
archive(native) = "pa_ppx_typedstatic.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_typedstatic.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx.base,camlp5.parser_quotations"
  archive(syntax,preprocessor,-native) = "pa_ppx_typedstatic.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_typedstatic.cmxa"

|} version)
