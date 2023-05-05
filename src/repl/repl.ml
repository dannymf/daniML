open Interp
module Env = Ast.Env
let interp_big_env = Interp.Eval.interp_big_env
(* let string_of_const = Interp.Printing.string_of_const *)

  let rec read_eval_print input_acc env =
    let prompt =
      if input_acc = "" then "> "
      else "| "
    in
    print_string prompt;
    flush stdout;
    try
      let input = " " ^ read_line () in
      if String.length input > 1 && input.[(String.length input) - 1] = ';' && input.[(String.length input) - 2] = ';' then begin
        let full_input = input_acc ^ (String.sub input 0 ((String.length input) - 2)) in
        let result, new_env = interp_big_env full_input env in
        (* let interp_typ = Interp.Main.typeof Interp.Main.Context.empty result in *)
        let result_str = begin
        match result with
        | Int x -> string_of_int x
        | Bool x -> string_of_bool x
        | Float x -> string_of_float x
        | Closure (_, _, _, typ, _) -> "closure " ^ Printing.string_of_typ typ
        | Prob _ -> "prob e"
        | Decl _ -> "decl"
        | _ -> "precondition violated"
        end
        (* let type_str = begin
          match interp_typ with
          | Interp.Main.TInt -> "int"
          | Interp.Main.TBool -> "bool"
        end *)
       (* in print_endline (result_str ^ " ::- " ^ type_str); flush stdout; read_eval_print "" *)
       in print_endline (result_str); flush stdout; read_eval_print "" new_env
      end 
     else read_eval_print (input_acc ^ input) env
    with
    (* | Lexer.Error msg -> print_endline msg; read_eval_print "" *)
    | Parser.Error -> print_endline "Error: invalid input"; read_eval_print "" env
    | End_of_file -> print_endline "Goodbye!"; exit 0
    | exn -> print_endline (Printexc.to_string exn); read_eval_print "" env
  
  let _ = read_eval_print "" Env.empty


