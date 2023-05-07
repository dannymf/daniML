open Interp.Eval
open Interp.Printing

let eval_exp expr_str = fun () ->
  Stdlib.Random.self_init ();
  interp_big expr_str

let run_n_times n f =
  for _ = 1 to n do
    let result = f () in
    print_endline (string_of_val result)
  done

let _ =
  let n = int_of_string Sys.argv.(1) in
  let expr_file = Sys.argv.(2) in
  let ic = open_in expr_file in
  let expr_str = really_input_string ic (in_channel_length ic) in
  close_in ic;
  print_newline ();
  run_n_times n (eval_exp expr_str)