{
open Lexing
open Parser
}

let white = [' ' '\t' '\r' '\n']+
let string = [^ '(' ')' '|' '\\' '.' ',' ':' '-' '<' '>' '=' ' ' '\t' '\r' '\n']+

rule token = parse
  | eof { EOF }
  | "(*" ([^'*'] | '*' [^')'])* "*)" { token lexbuf }
  | white { token lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "|" { PIPE }
  | "\\" { BACKSLASH }
  | "." { DOT }
  | "," { COMMA }
  | ":" { COLON }
  | "|>" { TRIANGLE }
  | "->" { ARROW }
  | "<->" { BIARROW }
  | "=" { EQUAL }
  | "let" { LET }
  | "in" { IN }
  | "iso" { ISO }
  | "fix" { FIX }
  | "type" { TYPE }
  | "invert" { INVERT }
  | "rec" { REC }
  | string { NAME (lexeme lexbuf) }

