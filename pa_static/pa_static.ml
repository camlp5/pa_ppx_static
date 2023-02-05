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
  ; static_preamble : MLast.str_item list
  ; statics : str_item list ref
  ; prefix : string
  ; counter : int ref
  }

let mk prefix preamble = {
    exprs = ExprHash.create 23
  ; static_preamble = preamble
  ; prefix
  ; counter = ref 0
  ; statics = ref []
  }

let add it e =
  match ExprHash.find it.exprs (reloc_expr e) with
    n -> n
  | exception Not_found ->
     let loc = loc_of_expr e in
     let n = !(it.counter) in
     incr it.counter ;
     let name = Fmt.(str "%s_%d__" it.prefix n) in
     let si = <:str_item< let $lid:name$ = Pa_ppx_static.Runtime.Static.mk (fun () -> $e$) >> in
     Std.push it.statics si ;
     ExprHash.add it.exprs (reloc_expr e) name ;
     name

let all it = it.static_preamble @ (List.rev !(it.statics))

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
  let (preamble_sil, sil)  = match sil with
      (<:str_item< [%%static_preamble $structure:l$] >>, _)::tl -> (l,tl)
    | l -> ([], l) in
  let fname = Ctxt.filename arg in
  let static_name_prefix = "__static" in
  init arg (Statics.mk static_name_prefix preamble_sil) ;
  (sil, status)

let finish_implem arg z =
  let (sil, status) = z in
  let sil0 = all_statics arg in
  if sil0 = [] then
    (sil, status)
  else
    let loc = MLast.loc_of_str_item (List.hd sil0) in
    let si = <:str_item:< open(struct $list:sil0$ end) >> in
    ([si,loc]@sil, status)

let rewrite_static arg = function
  <:expr:< [%static $exp:e$ ] >> as e0 ->
   let sname = add_static arg e in
   <:expr< Pa_ppx_static.Runtime.Static.get $lid:sname$ >>
| _ -> assert false

let pp_str_item msg ty =
  Fmt.(pf stderr "%s: #<str_item< %s >>\n%!" msg (Eprinter.apply Pcaml.pr_str_item Pprintf.empty_pc ty))

let wrap_top_phrase arg z =
  match z with
    None -> None
  | Some si ->
(*
     pp_str_item "before" si ;
 *)
     let fname = Ctxt.filename arg in
     let static_name_prefix = "__static" in
     init arg (Statics.mk static_name_prefix []) ;
     z

let finish_top_phrase arg z =
  match z with
    None -> None
  | Some si ->
(*
     pp_str_item "after(1)" si ;
 *)
     let sil0 = all_statics arg in
     if sil0 = [] then
       z
  else
    let sil0 = all_statics arg in
    if sil0 = [] then z
    else
      let loc = MLast.loc_of_str_item (List.hd sil0) in
      let si = match si with
          <:str_item< $exp:e$ >> ->
             <:str_item< let open(struct $list:sil0$ end) in $e$ >>
        | _ -> <:str_item:< open(struct $list:sil0@[si]$ end) >> in
(*
      pp_str_item "after(2)" si ;
 *)
      Some si

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

let ef = EF.{ (ef) with
              top_phrase = extfun ef.top_phrase with [
    z ->
    fun arg fallback -> 
      Some (z |> wrap_top_phrase arg |> fallback arg |> finish_top_phrase arg)
  ] } in


  Pa_passthru.(install { name = "pa_static"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;
