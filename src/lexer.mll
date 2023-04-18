{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let letter_num = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let id = letter+ letter_num*

rule read = 
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  (* | "random" { RANDOM } *)
  | "<=" { LEQ }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { MINUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "->" { ARROW }
  | "fun" { FUN }
  | "fix" { FIX }
  (* | "let fix" {LETFIX} *)
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }