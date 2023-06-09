{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let int = digit+
let float = digit* frac exp?
let letter = ['a'-'z' 'A'-'Z']
let letter_num = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let id = letter+ letter_num*

rule read = 
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
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
  | "=>" { ARROW }
  | "->" { TARROW }
  | "fun" { FUN }
  (* | "lamb" { FUN } *)
  | "let fix" { LETFIX }
  (* | "let fix" {LETFIX} *)

  (* TYPES *)
  | ":" { COLON }
  | "int" { INT_TYPE }
  | "bool" { BOOL_TYPE }
  | "float" { FLOAT_TYPE }

  (* PROBABILISTIC *)
  | "S" { RANDOM }
  | "sample" { SAMPLE }
  | "from" { FROM }
  | "prob" { PROB }
  | "unprob" { UNPROB }

  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }