open Interp
open Interp.Eval
open Ast
open Interp.Printing

let uniform_exec = 
  "let uniform = fun a:float => fun b:float =>
                 sample x from prob S in
                 a + x * (b - a) in
    uniform 0. 100."

let bernoulli = "let bernoulli =  fun p:float =>
                                  prob
                                  sample x from prob S in
                                  x <= p in 
                                  bernoulli 0.5"

let geometric = 
  "let bernoulli = fun p:float =>
    prob
    sample x from prob S in
    x <= p in
  let fix geometric p:float => float =
    let bernoulli_p = bernoulli p in
    sample b from bernoulli_p in
    sample y from 
    (if b then prob 0 else
      prob sample x from geometric p in (1. + x))
    in y in geometric 0.5"

let sample_gen = 
  "sample x from prob S in x"

let geom_rec =
  "let bernoulli = fun pr:float => 
    prob sample x from prob S in x <= pr in
    let fix geometric : float -> prob int = fun p:float => 
      prob sample b from bernoulli p in
      if b then 0 else sample x from geometric p in 1 + x in
      unprob geometric .00001"


let testtest =
  "let testtest = fun x:int => fun y:int =>
  let fix factorial : int -> int =
    fun n:int => if n <= 1 then 1 else n * factorial (n - 1)
  in factorial x + y in testtest 5 7"
let eval_exp strname = fun () -> 
  Stdlib.Random.self_init ();
  interp_big strname

(* let time_it (f: unit => 'a) = fun () =>
  let t1 = Unix.gettimeofday () in
  let result = f () in
  let t2 = Unix.gettimeofday () in
  let diff = t2 -. t1 in
  Printf.eprintf "Time taken: %f seconds\n" diff;
  result *)

let run_n_times (n: int) (f: unit -> expr) =
  for _ = 1 to n do
    let result = f () in
    print_endline (string_of_val result)
  done

let _ = 
  let n = 10 in
  run_n_times n (eval_exp testtest)
