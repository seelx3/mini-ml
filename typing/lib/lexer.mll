{
  open Parser
}

let digit = ['0'-'9']
let integer = digit+
let whitespace = [' ' '\t' '\r' '\n']

rule token = parse
  | whitespace         { token lexbuf }
  | integer as i       { INT (int_of_string i) }
  | "true"             { TRUE }
  | "false"            { FALSE }
  | '+'                { PLUS }
  | '<'                { LT }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | "if"               { IF }
  | "then"             { THEN }
  | "else"             { ELSE }
  | eof                { EOF }
  | _                  { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
