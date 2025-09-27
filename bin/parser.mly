%{
open Types
%}

%token EOF
%token LPAREN
%token RPAREN
%token PIPE
%token BACKSLASH
%token DOT
%token COMMA
%token BIARROW
%token EQUAL
%token TIMES
%token LET
%token IN
%token ISO
%token END
%token FIX
%token TYPE
%token INVERT
%token OF
%token <string> NAME

%start <program> program
%type <base_type> base_type
%type <value> value
%type <pat> pat
%type <expr> expr
%type <iso> iso
%type <term> term
%type <variant> variant
%type <typedef> typedef
%type <value * expr> biarrowed
%%

program:
  | ts = typedef*; t = term; EOF; { { ts; t } }

typedef:
  | TYPE; t = NAME; EQUAL; PIPE?; vs = separated_nonempty_list(PIPE, variant); { { t; vs } }

variant:
  | c = NAME; OF; a = base_type; { Iso { c; a } }
  | c = NAME; { Value c }

base_type:
  | LPAREN; ts = separated_list(TIMES, base_type); RPAREN;
    {
      match ts with
      | [] -> Unit
      | [a] -> a
      | _ -> Product ts
    }
  | x = NAME; { Named x }

value:
  | LPAREN; vs = separated_list(COMMA, value); RPAREN;
    {
      match vs with
      | [] -> Unit
      | [v] -> v
      | _ -> Tuple vs
    }
  | c = NAME; v = value; { Cted { c; v } }
  | x = NAME; { Named x }

pat:
  | x = NAME; { Named x }
  | LPAREN; ps = separated_nonempty_list(COMMA, pat); RPAREN;
    {
      match ps with
      | [] -> failwith "unreachable"
      | [p] -> p
      | _ -> Tuple ps
    }

expr:
  | v = value; { Value v }
  | LET; p_1 = pat; EQUAL; omega = iso; p_2 = pat; IN; e = expr; { Let { p_1; omega; p_2; e } }

biarrowed:
  | v = value; BIARROW; e = expr; { (v, e) }

iso:
  | LPAREN; omega = iso; RPAREN; { omega }
  | ISO; PIPE?; pairs = separated_nonempty_list(PIPE, biarrowed); END; { Pairs pairs }
  | FIX; phi = NAME; DOT; omega = iso; { Fix { phi; omega } }
  | BACKSLASH; psi = NAME; DOT; omega = iso; { Lambda { psi; omega } }
  | x = NAME; { Named x }
  | omega_1 = iso; omega_2 = iso; { App { omega_1; omega_2 } }
  | INVERT; omega = iso; { Invert omega }

term:
  | LPAREN; ts = separated_list(COMMA, term); RPAREN;
    {
      match ts with
      | [] -> Unit
      | [t] -> t
      | _ -> Tuple ts
    }
  | c = NAME; t = term; { Cted { c; t } }
  | x = NAME; { Named x }
  | omega = iso; t = term; { App { omega; t } }
  | LET; p = pat; EQUAL; t_1 = term; IN; t_2 = term; { Let { p; t_1; t_2 } }
