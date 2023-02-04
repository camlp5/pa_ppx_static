(** -syntax camlp5o *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil

module Statics = struct
open MLast
type t = {
    statics : (str_item * loc) list ref
  ; prefix : string
  ; counter : int ref
  }

let mk prefix = { prefix ; counter = ref 0 ; statics = ref [] }

let add it e =
  let loc = loc_of_expr e in
  let n = !(it.counter) in
  incr it.counter ;
  let name = Fmt.(str "%s_%d__" it.prefix n) in
  let si = <:str_item< let $lid:name$ = Pa_ppx_typedstatic.Runtime.Static.mk (fun () -> $e$) >> in
  Std.push it.statics (si, loc_of_str_item si) ;
  name

let all it = !(it.statics)

end

type scratchdata_t += Pa_typedstatic of Statics.t

let add_static arg e =
  match Ctxt.refscratchdata arg "typedstatic" with
    Pa_typedstatic ctxt -> Statics.add ctxt e
  | _ -> assert false

let init arg it =
   Ctxt.init_refscratchdata arg "typedstatic" (Pa_typedstatic it)

let all_statics arg =
  match Ctxt.refscratchdata arg "typedstatic" with
    Pa_typedstatic ctxt -> Statics.all ctxt
  | _ -> assert false

let init arg it =
   Ctxt.init_refscratchdata arg "typedstatic" (Pa_typedstatic it)

let wrap_implem arg z =
  let (sil, status) = z in
  let fname = Ctxt.filename arg in
  let hexs = Digest.(fname |> string |> to_hex) in
  let static_name_prefix = Fmt.(str "__static_%s" hexs) in
  init arg (Statics.mk static_name_prefix) ;
  (sil, status)

let finish_implem arg z =
  let (sil, status) = z in
  let sil0 = all_statics arg in
  (sil0@sil, status)

let rewrite_typedstatic arg = function
  <:expr:< [%typedstatic ( $e$ : $t$) ] >> as e0 ->
   let sname = add_static arg e in
   <:expr< Pa_ppx_typedstatic.Runtime.Static.get $lid:sname$ >>
| _ -> assert false

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%typedstatic $exp:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_typedstatic arg z)
  ] } in

let ef = EF.{ (ef) with
              implem = extfun ef.implem with [
    z ->
    fun arg fallback -> 
      Some (z |> wrap_implem arg |> fallback arg |> finish_implem arg)
  ] } in


  Pa_passthru.(install { name = "pa_typedstatic"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;
