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
%token TRIANGLE
%token ARROW
%token BIARROW
%token EQUAL
%token LET
%token IN
%token ISO
%token FIX
%token TYPE
%token INVERT
%token REC
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
%right ARROW
%%

wtf(separator, X):
  | x = X; separator; y = X; { [x; y] }
  | x = X; separator; xs = wtf(separator, X) { x :: xs }

program:
  | ts = typedef*; t = term; EOF; { { ts; t } }

typedef:
  | TYPE; t = NAME; EQUAL; PIPE?; vs = separated_nonempty_list(PIPE, variant); { { t; vs } }

variant:
  | c = NAME; a = base_type; { Iso { c; a } }
  | c = NAME; { Value c }

base_type:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; ts = wtf(COMMA, base_type) RPAREN; { Product ts }
  | x = NAME; { Named x }

iso_type:
  | LPAREN; t = iso_type; RPAREN; { t }
  | a = base_type; BIARROW; b = base_type; { BiArrow { a; b } }
  | t_1 = iso_type; ARROW; t_2 = iso_type; { Arrow { t_1; t_2 } }

value:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; vs = wtf(COMMA, value); RPAREN; { Tuple vs }
  | c = NAME; v = value; { Cted { c; v } }
  | x = NAME; { Named x }

pat:
  | x = NAME; { Named x }
  | LPAREN; ps = wtf(COMMA, pat); RPAREN; { Tuple ps }

expr:
  | v = value; { Value v }
  | LET; p_1 = pat; EQUAL; omega = iso; p_2 = pat; IN; e = expr; { Let { p_1; omega; p_2; e } }
  | LET; p_1 = pat; EQUAL; p_2 = pat; TRIANGLE; omega = iso; IN; e = expr; { Let { p_1; omega; p_2; e } }

biarrowed:
  | v = value; BIARROW; e = expr; { (v, e) }

iso:
  | LPAREN; omega = iso; RPAREN; { omega }
  | ISO; COLON; anot = iso_type; DOT; PIPE?; pairs = separated_nonempty_list(PIPE, biarrowed);
    { Pairs { anot; pairs } }

  | REC; phi = NAME; COLON; anot = iso_type; DOT; PIPE?; pairs = separated_nonempty_list(PIPE, biarrowed);
    { Fix { phi; anot; omega = Pairs { anot; pairs } } }

  | FIX; phi = NAME; COLON; anot = iso_type; DOT; omega = iso; { Fix { phi; anot; omega } }
  | BACKSLASH; psi = NAME; COLON; anot = iso_type; DOT; omega = iso; { Lambda { psi; anot; omega } }
  | x = NAME; { Named x }
  | omega_1 = iso; omega_2 = iso; { App { omega_1; omega_2 } }
  | INVERT; omega = iso; { Invert omega }

term:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; ts = wtf(COMMA, term); RPAREN; { Tuple ts }
  | x = NAME; { Named x }
  | omega = iso; t = term; { App { omega; t } }
  | LET; p = pat; EQUAL; t_1 = term; IN; t_2 = term; { Let { p; t_1; t_2 } }
  | LET; ISO; phi = NAME; EQUAL; omega = iso; IN; t = term { LetIso { phi; omega; t } }
