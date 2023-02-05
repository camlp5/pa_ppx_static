(** -syntax camlp5o *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil

let reloc_expr e = Reloc.expr (fun _ -> Ploc.dummy) 0 e

module ExprHash = Hashtbl.Make(struct
                      type t = MLast.expr
                      let equal = Reloc.eq_expr
                      let hash e = Hashtbl.hash (reloc_expr e)
                    end)

module Statics = struct
open MLast
type t = {
    exprs : string ExprHash.t
  ; statics : (str_item * loc) list ref
  ; prefix : string
  ; counter : int ref
  }

let mk prefix = { exprs = ExprHash.create 23 ; prefix ; counter = ref 0 ; statics = ref [] }

let add it e =
  match ExprHash.find it.exprs (reloc_expr e) with
    n -> n
  | exception Not_found ->
     let loc = loc_of_expr e in
     let n = !(it.counter) in
     incr it.counter ;
     let name = Fmt.(str "%s_%d__" it.prefix n) in
     let si = <:str_item< let $lid:name$ = Pa_ppx_static.Runtime.Static.mk (fun () -> $e$) >> in
     Std.push it.statics (si, loc_of_str_item si) ;
     ExprHash.add it.exprs (reloc_expr e) name ;
     name

let all it = !(it.statics)

end

type scratchdata_t += Pa_static of Statics.t

let add_static arg e =
  match Ctxt.refscratchdata arg "static" with
    Pa_static ctxt -> Statics.add ctxt e
  | _ -> assert false

let init arg it =
   Ctxt.init_refscratchdata arg "static" (Pa_static it)

let all_statics arg =
  match Ctxt.refscratchdata arg "static" with
    Pa_static ctxt -> Statics.all ctxt
  | _ -> assert false

let init arg it =
   Ctxt.init_refscratchdata arg "static" (Pa_static it)

let wrap_implem arg z =
  let (sil, status) = z in
  let fname = Ctxt.filename arg in
  let hexs = Digest.(fname |> string |> to_hex) in
  let static_name_prefix = Fmt.(str "__static_%s" hexs) in
  init arg (Statics.mk static_name_prefix) ;
  (sil, status)

let finish_implem arg z =
  let (sil, status) = z in
  let sil0 = List.rev (all_statics arg) in
  (sil0@sil, status)

let rewrite_static arg = function
  <:expr:< [%static $exp:e$ ] >> as e0 ->
   let sname = add_static arg e in
   <:expr< Pa_ppx_static.Runtime.Static.get $lid:sname$ >>
| _ -> assert false

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%static $exp:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_static arg z)
  ] } in

let ef = EF.{ (ef) with
              implem = extfun ef.implem with [
    z ->
    fun arg fallback -> 
      Some (z |> wrap_implem arg |> fallback arg |> finish_implem arg)
  ] } in


  Pa_passthru.(install { name = "pa_static"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;
