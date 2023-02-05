(** -syntax camlp5o *)
[%%static_preamble open Rresult]
open OUnit2


let test_simple ctxt =
  ()
  ; [%static ()]
  ; [%static ()]
  ; [%static ()]

let suite = "Test pa_ppx_static" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

