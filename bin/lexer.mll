{
open Lexing
open Parser
}

let white = [' ' '\t' '\r' '\n']+
let string = [^ '(' ')' '{' '}' '|' '\\' '.' ',' ':' '-' '<' '>' '=' ' ' '\t' '\r' '\n']+

rule token = parse
  | eof { EOF }
  | "(*" ([^'*'] | '*' [^')'])* "*)" { token lexbuf }
  | white { token lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
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
  | "of" { OF }
  | string
    {
      let x = lexeme lexbuf in
      if Util.is_type_variable x then TVAR (int_of_string x)
      else if Util.is_variable x then VAR x
      else CTOR x
    }

