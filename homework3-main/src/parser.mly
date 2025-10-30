%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token TRUE FALSE
%token PLUS TIMES
%token LEQ
%token LET EQUALS IN
%token IF THEN ELSE
%token LPAREN RPAREN
%token EOF

%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left TIMES

%start main
%type <Ast.expr> main

%%

main:
  | expr EOF { $1 }

expr:
  | INT                          { Int $1 }
  | ID                           { Var $1 }
  | TRUE                         { Bool true }
  | FALSE                        { Bool false }
  | expr PLUS expr               { Binop (Plus, $1, $3) }
  | expr TIMES expr              { Binop (Times, $1, $3) }
  | expr LEQ expr                { Binop (Leq, $1, $3) }
  | LET ID EQUALS expr IN expr   { Let ($2, $4, $6) }
  | IF expr THEN expr ELSE expr  { If ($2, $4, $6) }
  | LPAREN expr RPAREN           { $2 }
;
