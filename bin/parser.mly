%{
open Types
%}

%token EOF
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
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
%token OF
%token <int> TVAR
%token <string> VAR
%token <string> CTOR

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
%type <string> param
%right ARROW
%%

wtf(separator, X):
  | x = X; separator; y = X; { [x; y] }
  | x = X; separator; xs = wtf(separator, X) { x :: xs }

program:
  | ts = typedef*; t = term; EOF; { { ts; t } }

typedef:
  | TYPE; t = VAR; EQUAL; PIPE?; vs = separated_nonempty_list(PIPE, variant); { { t; vs } }

variant:
  | c = CTOR; OF; a = base_type; { Iso { c; a } }
  | c = CTOR; { Value c }

base_type:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; ts = wtf(COMMA, base_type) RPAREN; { Product ts }
  | x = VAR; { Named x }

iso_type:
  | LPAREN; t = iso_type; RPAREN; { t }
  | a = base_type; BIARROW; b = base_type; { BiArrow { a; b } }
  | t_1 = iso_type; ARROW; t_2 = iso_type; { Arrow { t_1; t_2 } }

value:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; v = value; RPAREN; { v }
  | LPAREN; vs = wtf(COMMA, value); RPAREN; { Tuple vs }
  | c = CTOR; v = value; { Cted { c ; v } }
  | x = VAR; { Named x }
  | x = CTOR; { Named x }

pat:
  | x = VAR; { Named x }
  | LPAREN; ps = wtf(COMMA, pat); RPAREN; { Tuple ps }

expr:
  | v = value; { Value v }
  | LET; p_1 = pat; EQUAL; omega = iso; p_2 = pat; IN; e = expr; { Let { p_1; omega; p_2; e } }

biarrowed:
  | v = value; BIARROW; e = expr; { (v, e) }

iso:
  | LBRACE; omega = iso; RBRACE; { omega }
  | ISO; PIPE?; p = separated_nonempty_list(PIPE, biarrowed); { Pairs p }
  | FIX; phi = VAR; DOT; omega = iso; { Fix { phi; omega } }
  | BACKSLASH; psi = VAR; DOT; omega = iso; { Lambda { psi; omega } }
  | x = VAR; { Named x }
  | x = CTOR; { Named x }
  | omega_1 = iso; omega_2 = iso; { App { omega_1; omega_2 } }
  | INVERT; omega = iso; { Invert omega }

param:
  | phi = VAR; { phi }

term:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; t = term; RPAREN; { t }
  | LPAREN; ts = wtf(COMMA, term); RPAREN; { Tuple ts }
  | x = VAR; { Named x }
  | x = CTOR; { Named x }
  | omega = iso; t = term; { App { omega; t } }
  | LET; p = pat; EQUAL; t_1 = term; IN; t_2 = term; { Let { p; t_1; t_2 } }
  | LET; ISO; phi = VAR; params = param*; EQUAL; omega = iso; IN; t = term;
    { LetIso { phi; omega = lambdas_of_params params omega; t } }

  | LET; REC; phi = VAR; params = param*; EQUAL; omega = iso; IN; t = term;
    {
      let omega = Fix { phi; omega } in
      LetIso { phi; omega = lambdas_of_params params omega; t }
    }
