exception TypeError of string
exception RuntimeError of string

let typ_error_test s =
  TypeError s
let type_error s =
  raise (TypeError s)

let runtime_error s =
  raise (RuntimeError s)

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(** The error message produced if binary operators and their
    operands do not have the correct types. *)
let bop_err = "Operator and operand type mismatch"

(** The error message produced if the [then] and [else] branches
    of an [if] do not have the same type. *)
let if_branch_err = "Branches of if must have same type"

(** The error message produced if the guard
    of an [if] does not have type [bool]. *)
let if_guard_err = "Guard of if must have type bool"

(** The error message produced if the first argument of a function application 
    does not have type [fun]. *)
let app_err = "The first expression must have type fun or fix"

let annotation_error = "Let expression type mismatch"