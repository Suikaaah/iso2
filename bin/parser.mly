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
%token COLON
%token ARROW
%token BIARROW
%token EQUAL
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
%type <iso_type> iso_type
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
  | LPAREN; ts = separated_list(COMMA, base_type); RPAREN;
    {
      match ts with
      | [] -> Unit
      | [a] -> a
      | _ -> Product ts
    }
  | x = NAME; { Named x }

iso_type:
  | LPAREN; t = iso_type; RPAREN; { t }
  | a = base_type; BIARROW; b = base_type; { BiArrow { a; b } }
  | t_1 = iso_type; ARROW; t_2 = iso_type; { Arrow { t_1; t_2 } }

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
  | ISO; COLON; anot = iso_type; DOT; PIPE?; pairs = separated_nonempty_list(PIPE, biarrowed); END; { Pairs { anot; pairs } }
  | FIX; phi = NAME; COLON; anot = iso_type; DOT; omega = iso; END; { Fix { phi; anot; omega } }
  | BACKSLASH; psi = NAME; COLON; anot = iso_type; DOT; omega = iso; END; { Lambda { psi; anot; omega } }
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
  | x = NAME; { Named x }
  | omega = iso; t = term; { App { omega; t } }
  | LET; p = pat; EQUAL; t_1 = term; IN; t_2 = term; { Let { p; t_1; t_2 } }
