{
open Lexing
open Parser
}

let white = [' ' '\t' '\r' '\n']+
let string = [^ '(' ')' '|' '\\' '.' ',' '-' '<' '>' '=' '*' ' ' '\t' '\r' '\n']+

rule token = parse
  | eof { EOF }
  | white { token lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "|" { PIPE }
  | "\\" { BACKSLASH }
  | "." { DOT }
  | "," { COMMA }
  | "<->" { BIARROW }
  | "=" { EQUAL }
  | "*" { TIMES }
  | "let" { LET }
  | "in" { IN }
  | "iso" { ISO }
  | "end" { END }
  | "fix" { FIX }
  | "type" { TYPE }
  | "invert" { INVERT }
  | "of" { OF }
  | string { NAME (lexeme lexbuf) }

