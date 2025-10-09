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
%token OF
%token CONS
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
%type <string * iso_type> param
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
  | LPAREN; vs = wtf(COMMA, value); RPAREN; { Tuple vs }
  | c = CTOR; v = value; { Cted { c ; v } }
  | v_1 = value; CONS; v_2 = value; { Cted { c = "Cons"; v = Tuple [v_1; v_2] } }
  | x = VAR; { Named x }
  | x = CTOR; { Named x }

pat:
  | x = VAR; { Named x }
  | LPAREN; ps = wtf(COMMA, pat); RPAREN; { Tuple ps }

expr:
  | v = value; { Value v }
  | LET; p_1 = pat; EQUAL; omega = iso; p_2 = pat; IN; e = expr; { Let { p_1; omega; p_2; e } }
  | LET; p_1 = pat; EQUAL; p_2 = pat; TRIANGLE; omega = iso; IN; e = expr; { Let { p_1; omega; p_2; e } }

biarrowed:
  | v = value; BIARROW; e = expr; { (v, e) }

iso:
  | LPAREN; omega = iso; RPAREN; { omega }
  | ISO; COLON; annot = iso_type; DOT; PIPE?; pairs = separated_nonempty_list(PIPE, biarrowed);
    { Pairs { annot; pairs } }

  | REC; phi = VAR; COLON; annot = iso_type; DOT; PIPE?; pairs = separated_nonempty_list(PIPE, biarrowed);
    { Fix { phi; annot; omega = Pairs { annot; pairs } } }

  | FIX; phi = VAR; COLON; annot = iso_type; DOT; omega = iso; { Fix { phi; annot; omega } }
  | BACKSLASH; psi = VAR; COLON; annot = iso_type; DOT; omega = iso; { Lambda { psi; annot; omega } }
  | x = VAR; { Named x }
  | x = CTOR; { Named x }
  | omega_1 = iso; omega_2 = iso; { App { omega_1; omega_2 } }
  | INVERT; omega = iso; { Invert omega }

param:
  | LPAREN; phi = VAR; COLON; t = iso_type; RPAREN; { (phi, t) }

term:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; ts = wtf(COMMA, term); RPAREN; { Tuple ts }
  | x = VAR; { Named x }
  | x = CTOR; { Named x }
  | omega = iso; t = term; { App { omega; t } }
  | t_1 = term; CONS; t_2 = term; { App { omega = Named "Cons"; t = Tuple [t_1; t_2] } }
  | LET; p = pat; EQUAL; t_1 = term; IN; t_2 = term; { Let { p; t_1; t_2 } }
  | LET; ISO; phi = VAR; params = param*; EQUAL; omega = iso; IN; t = term;
    { LetIso { phi; omega = lambdas_of_params params omega; t } }

  | LET; REC; phi = VAR; params = param*; COLON; annot = iso_type; EQUAL; omega = iso; IN; t = term;
    {
      let omega = Fix { phi; annot; omega } in
      LetIso { phi; omega = lambdas_of_params params omega; t }
    }

  | LET; ISO; phi = VAR; params = param*; COLON; annot = iso_type; EQUAL;
    PIPE?; pairs = separated_nonempty_list(PIPE, biarrowed); IN; t = term
    {
      let pairs = Pairs { annot; pairs } in
      LetIso { phi; omega = lambdas_of_params params pairs; t }
    }

  | LET; REC; phi = VAR; params = param*; COLON; annot = iso_type; EQUAL;
    PIPE?; pairs = separated_nonempty_list(PIPE, biarrowed); IN; t = term
    {
      let pairs = Fix { phi; annot; omega = Pairs { annot; pairs } } in
      LetIso { phi; omega = lambdas_of_params params pairs; t }
    }
