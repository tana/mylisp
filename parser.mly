%{
  open Evaluator
%}
%token <int> INT
%token <string> SYMBOL
%token LPAREN RPAREN
%token SPACE
%token EOL
%start main
%type <Evaluator.expr> main
%%
main:
    expr EOL  { $1 }
;
expr:
    atom { $1 }
  | list { $1 }
;
atom:
    SYMBOL { Symbol $1 }
  | INT { Int $1 }
;
list:
    LPAREN members RPAREN { List $2 }
  | LPAREN RPAREN { List [] }
;
members:
    expr  { [$1] }
  | expr SPACE members { $1::$3 }
;
