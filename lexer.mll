{
  open Parser
  open Evaluator
  exception Eof
}
rule token = parse
    [' ' '\t']+  { SPACE }
  | ['\n']  { EOL }
  | ['0'-'9']+  { INT(int_of_string(Lexing.lexeme lexbuf)) }
  | ['a'-'z' 'A'-'Z' '@' '!' '#'-'&' '*' '+' '-'-'/' '<'-'?']+  { SYMBOL(Lexing.lexeme lexbuf) }
  | ['('] { LPAREN }
  | [')'] { RPAREN }
  | eof { raise Eof }
