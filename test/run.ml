open Interp
open Interp.Eval
open Ast
open Interp.Printing

let uniform_exec = 
  "let uniform = fun a:float -> fun b:float ->
                 sample x from prob S in
                 a + x * (b - a) in
    uniform 0. 100."

let bernoulli = "let bernoulli =  fun p:float ->
                                  prob
                                  sample x from prob S in
                                  x <= p in 
                                  bernoulli 0.5"

let geometric = 
  "let bernoulli = fun p:float ->
    prob
    sample x from prob S in
    x <= p in
  let fix geometric p:float -> float =
    let bernoulli_p = bernoulli p in
    sample b from bernoulli_p in
    sample y from 
    (if b then prob 0 else
      prob sample x from geometric p in (1. + x))
    in y in geometric 0.5"

let geofake = 
  "let bernoulli = fun p:float ->
    prob
    sample x from prob S in
    x <= p in
  let fix geometric p:float -> float =
    let bernoulli_p = bernoulli p in (sample b from bernoulli_p 
      in (sample y from 
      (if b then prob 0 else prob (sample x from (geometric p) in (1. + x))) in y)) 
  in geometric 0.7"

let sample_gen = 
  "sample x from prob S in x"

let eval_exp strname = fun () -> 
  Stdlib.Random.self_init ();
  interp_big strname

(* let time_it (f: unit -> 'a) = fun () ->
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
  let n = 100000 in
  run_n_times n (eval_exp sample_gen)
