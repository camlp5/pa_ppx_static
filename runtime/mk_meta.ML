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
# Specifications for the "pa_ppx_typedstatic_runtime" package:
requires = "fmt,re,pcre,pa_ppx.base"
version = "%s"
description = "pa_ppx_typedstatic runtime support"

# For linking
archive(byte) = "pa_ppx_typedstatic_runtime.cma"
archive(native) = "pa_ppx_typedstatic_runtime.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_typedstatic_runtime.cma"

|} version)
