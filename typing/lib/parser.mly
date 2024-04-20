%{
  open Syntax
%}

%token <int> INT
%token <bool> BOOL

%token PLUS 
%token LT 

%token LPAREN 
%token RPAREN 

%token TRUE
%token FALSE
%token IF 
%token THEN 
%token ELSE 

%token EOF

%nonassoc ELSE
%left LT
%left PLUS MINUS
%left INT TRUE FALSE LPAREN

%start main
%type <Syntax.prog> main

%%

main:
  | expr EOF { $1 }
;

expr:
  | INT { Int($1) }
  | TRUE { Bool(true) }
  | FALSE { Bool(false) }
  | expr PLUS expr { Add($1, $3) }
  | LPAREN expr PLUS expr RPAREN { Add($2, $4) }
  | expr LT expr { Lt($1, $3) }
  | IF expr THEN expr ELSE expr { If($2, $4, $6) }
  ;