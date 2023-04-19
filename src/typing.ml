open Ast
open Errors
open Context

(** [typeof ctx e] is the type of [e] in context [ctx]. 
    Raises: [Failure] if [e] is not well typed in [ctx]. *)
let rec typeof ctx = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Closure (name, x, typ, e, _) -> typeof_rec ctx name x typ e
  | Var x -> ContextMap.lookup ctx x
  | Let (x, e1, e2) -> typeof_let ctx x e1 e2
  | Binop (bop, e1, e2) -> typeof_bop ctx bop e1 e2
  | If (e1, e2, e3) -> typeof_if ctx e1 e2 e3
  | Fun (x, typ, e) -> typeof_fun ctx x typ e
  | Rec (name, x, typ, e) -> typeof_rec ctx name x typ e
  | App (e1, e2) -> typeof_app ctx e1 e2

(** Helper function for [typeof]. *)
and typeof_bop ctx bop e1 e2 =
  let t1, t2 = typeof ctx e1, typeof ctx e2 in
  match bop, t1, t2 with
  | Add, TInt, TInt 
  | Mult, TInt, TInt 
  | Minus, TInt, TInt -> TInt
  | Leq, TInt, TInt -> TBool
  | _ -> type_error Errors.bop_err

(** Helper function for [typeof]. *)
and typeof_let ctx x e1 e2 = 
  let t' = typeof ctx e1 in
  let ctx' = ContextMap.extend ctx x t' in
  typeof ctx' e2

(** Helper function for [typeof]. *)
and typeof_if ctx e1 e2 e3 =
  match typeof ctx e1 with
  | TBool ->
    let t2 = typeof ctx e2 in
    if t2 = typeof ctx e3 then t2
    else type_error Errors.if_branch_err
  | _ -> type_error Errors.if_guard_err

and typeof_app ctx e1 e2 =
  match typeof ctx e1, typeof ctx e2 with
  | TArrow (typ1, typ2), typ3 when typ1 = typ3 ->
    typ2
  | _ -> type_error Errors.app_err


and typeof_fun ctx x typ e =
  let ctx' = ContextMap.extend ctx x typ in
  let t1 = typeof ctx' e in
  TArrow (typ, t1)

and typeof_rec ctx name x typ e =
  let ctx' = ContextMap.extend ctx x typ in
  let ctx'' = ContextMap.extend ctx' name (TArrow (typ, typeof ctx' e)) in
  let t1 = typeof ctx'' e in
  TArrow (typ, t1)

(** [typecheck e] checks whether [e] is well typed in
    the empty context. Raises: [Failure] if not. *)
let typecheck e =
  ignore (typeof ContextMap.empty e); e