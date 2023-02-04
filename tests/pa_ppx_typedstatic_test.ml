(** -syntax camlp5o *)
open OUnit2


let test_simple ctxt =
  ()
  ; [%typedstatic (() : unit)]

let suite = "Test pa_ppx_typedstatic" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

