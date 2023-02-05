(** -syntax camlp5o *)
[%%static_preamble open List]
open OUnit2

let hd x = 2

let test_simple ctxt =
  ()
  ; [%static ()]
  ; assert_equal 2 (hd [1;2;3])
  ; assert_equal 1 [%static hd [1;2;3]]

let suite = "Test pa_ppx_static" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

