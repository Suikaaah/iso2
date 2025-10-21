{
open Lexing
open Parser
}

let white = [' ' '\t' '\r' '\n']+
let string = [^ '(' ')' '[' ']' '{' '}' '*' '|' '.' ',' ';' ':' '-' '<' '>' '=' ' ' '\t' '\r' '\n']+

rule token = parse
  | eof { EOF }
  | "(*" ([^'*'] | '*' [^')'])* "*)" { token lexbuf }
  | white { token lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "*" { TIMES }
  | "|" { PIPE }
  | "." { DOT }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | "::" { CONS }
  | "->" { ARROW }
  | "<->" { BIARROW }
  | "=" { EQUAL }
  | "unit" { UNIT }
  | "let" { LET }
  | "in" { IN }
  | "iso" { ISO }
  | "fix" { FIX }
  | "type" { TYPE }
  | "invert" { INVERT }
  | "rec" { REC }
  | "of" { OF }
  | "function" { FUNCTION }
  | "fun" { FUN }
  | "match" { MATCH }
  | "with" { WITH }
  | string
    {
      let x = lexeme lexbuf in
      if Util.is_type_variable x then TVAR x
      else if Util.is_variable x then VAR x
      else CTOR x
    }

