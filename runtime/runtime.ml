(** -syntax camlp5o *)
(* camlp5o *)
(* runtime.ml,v *)

open Pa_ppx_base
open Ppxutil

module Static = struct
  type 'a t = {
      it : 'a option ref
    ; createf : unit -> 'a
    }

  let mk createf = { it = ref None ; createf }
  let get it =
    match !(it.it) with
      Some v -> v
    | None ->
       let v = it.createf () in
       it.it := Some v ;
       v
end
