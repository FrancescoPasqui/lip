%{
open Ast
%}

%token <string> CONST
%token STAR
%token SLASH
%token PLUS
%token MINUS
%token LPAREN
%token RPAREN
%token EOF

%left STAR
%left SLASH
%left PLUS
%left MINUS

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | MINUS; e = expr { Neg(e)}
  | e1 = expr; STAR; e2 = expr { Mul(e1,e2) }
  | e1 = expr; SLASH; e2 = expr { Div(e1,e2) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;
