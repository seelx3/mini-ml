{
  open Parser
}

let digit = ['0'-'9']
let integer = digit+
let variable = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
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
  | ","                { COMMA }
  | "fst"              { FST }
  | "snd"              { SND }
  | "fun"              { FUN }
  | "->"               { ARROW }
  | "let"              { LET }
  | "="                { EQ }
  | "in"               { IN }
  | variable as v      { VAR v }
  | eof                { EOF }
  | _                  { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
