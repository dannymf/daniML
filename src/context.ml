open Ast
open Errors

(** A [Context] is a mapping from variable names to
    types, aka a symbol table, aka a typing environment. *)
module type Context = sig

  (** [t] is the type of a context. *)
  type t

  (** [empty] is the empty context. *)
  val empty : t

  (** [lookup ctx x] gets the binding of [x] in [ctx]. 
      Raises: [Failure unbound_var_err] if [x] is
      not bound in [ctx]. *) 
  val lookup : t -> string -> typ

  (** [extend ctx x ty] is [ctx] extended with a binding
      of [x] to [ty]. *)
  val extend : t -> string -> typ -> t
end

(** The [Context] module implements the [Context] signature 
    with an association list. *)
module ContextMap : Context = struct

  module Map = Map.Make(String)
  (* type t = (string * typ) list *)
  type t = typ Map.t

  let empty = Map.empty

  let lookup ctx x =
    try Map.find x ctx
    with Not_found -> type_error Errors.unbound_var_err

  let extend ctx x ty =
    Map.add x ty ctx
end