%{
  open Syntax
%}

%token <int> INT
%token <string> VAR

%token PLUS 
%token LT 

%token LPAREN 
%token RPAREN 

%token COMMA
%token FST
%token SND

%token TRUE
%token FALSE
%token IF 
%token THEN 
%token ELSE 

%token LET
%token EQ
%token IN

%token FUN
%token ARROW

%token EOF

%nonassoc THEN ELSE EQ IN
%left FST SND
%left LT COMMA
%left PLUS MINUS 
%left INT TRUE FALSE VAR LPAREN

%start main
%type <Syntax.prog> main

%%

main:
  | expr EOF { $1 }
;

arg_expr:
  | VAR { Var($1) }
  | INT { Int($1) }
  | TRUE { Bool(true) }
  | FALSE { Bool(false) }
  | LPAREN expr RPAREN { $2 }

expr:
  | arg_expr { $1 }
  | expr arg_expr { App($1, $2) }
  | expr PLUS expr { Add($1, $3) }
  | expr PLUS expr { Add($1, $3) }
  | expr COMMA expr { Product($1, $3) }
  | FST expr { Fst($2) }
  | SND expr { Snd($2) }
  | expr LT expr { Lt($1, $3) }
  | IF expr THEN expr ELSE expr { If($2, $4, $6) }
  | LPAREN RPAREN { Unit }
  | FUN VAR ARROW expr { Fun($2, $4) }
  | LET VAR EQ expr IN expr { Let($2, $4, $6) }
  ;