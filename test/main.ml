open OUnit2
open Interp
open Interp.Eval
open Ast
open Interp.Printing
open Interp.Errors

(* let interp_big = Interp.Main.interp_big *)

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Int i]. *)
let make_i n i s =
  [
    (* n >:: (fun _ -> assert_equal (Int i) (interp_small s)); *)
   n >:: (fun _ -> assert_equal (Int i) (interp_big s))]

(** [make_b n b s] makes an OUnit test named [n] that expects
    [s] to evalute to [Bool b]. *)
let make_b n b s =
  [
    (* n >:: (fun _ -> assert_equal (Bool b) (interp_small s)); *)
   n >:: (fun _ -> assert_equal (Bool b) (interp_big s))]

(** [make_t n s] makes an OUnit test named [n] that expects
    [s] to fail type checking with error string [s']. *)
let make_typerr n s' s =
  [
    (* n >:: (fun _ -> assert_raises (Failure s') (fun () -> interp_small s)); *)
   n >:: (fun _ -> assert_raises (typ_error_test s') (fun () -> interp_big s))]


(** [make_b n b s] makes an OUnit test named [n] that expects
    [s] to evalute to [String b]. *)
let make_s n (str:string) (s:string) =
  [
    n >:: (fun _ -> assert_equal str (string_of_typexp s))]

let tests = [
  make_i "int" 22 "22";
  make_i "add" 22 "11+11";
  make_i "adds" 22 "(10+1)+(5+6)";
  make_i "let" 22 "let x=22 in x";
  make_i "lets" 22 "let x = 0 in let x = 22 in x";
  make_i "mul1" 22 "2*11";
  make_i "mul2" 22 "2+2*10";
  make_i "mul3" 14 "2*2+10";
  make_i "mul4" 40 "2*2*10";
  make_i "if1" 22 "if true then 22 else 0";
  make_b "true" true "true";
  make_b "leq" true "1<=1";
  make_i "if2" 22 "if 1+2 <= 3+4 then 22 else 0";
  make_i "if3" 22 "if 1+2 <= 3*4 then let x = 22 in x else 0";
  make_b "letifpre" true "let x = 1+2 <= 3*4 in x";
  make_i "letif" 22 "let x = 1+2 <= 3*4 in if x then 22 else 0";
  make_i "rec1" 6 "let fix fact x : int -> int = if x <= 1 then 1 else x * fact (x - 1) in fact 3";
  make_i "fibrec" 55 "let fix fib x : int -> int = if x <= 0 then 0 else if x <= 1 then 1 else fib (x-1) + fib (x-2) in fib 10";
  make_i "doubarg" 40 "(fun x:int -> fun y:int -> x*y) 5 8";
]

let type_tests = [
  make_typerr "ty plus" bop_err "1 + true";
  make_typerr "ty mult" bop_err "1 * false";
  make_typerr "ty leq" bop_err "true <= 1";
  make_typerr "if guard" if_guard_err "if 1 then 2 else 3";
  make_typerr "if branch" if_branch_err "if true then 2 else false";
  make_typerr "unbound" unbound_var_err "x";
  make_s "typint1" "int" "3";
  make_s "typint2" "int" "482 + 903";
  make_s "typint3" "int" "(0-9021) * 32142";
  make_s "typbool1" "bool" "true";
  make_s "typbool2" "bool" "if false then true else false";
  make_s "typfun1" "(int -> int)" "fun x:int -> x * 5";
  make_s "typrec1" "int" "let fix fact x : int -> int = if x <= 1 then 1 else x * fact (x - 1) in fact 3";
  make_s "typrec2" "(int -> int)" "let fix fact x : int -> int = if x <= 1 then 1 else x * fact (x-1) in fact";
  make_s "typfib" "(int -> int)" "let fix fib x : int -> int = if x <= 0 then 0 else if x <= 1 then 1 else fib (x-1) + fib (x-2) in fib";
  make_s "typdoubarg" "(int -> (int -> int))" "fun x:int -> fun y:int -> x*y";
]

let _ = run_test_tt_main ("suite" >::: List.flatten (tests @ type_tests))