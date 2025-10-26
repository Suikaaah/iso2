%{
  open Types
%}

%token EOF
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token TIMES
%token PIPE
%token DOT
%token COMMA
%token SEMICOLON
%token CONS
%token ARROW
%token BIARROW
%token EQUAL
%token UNIT
%token LET
%token IN
%token ISO
%token FIX
%token TYPE
%token INVERT
%token REC
%token OF
%token FUN
%token CASE
%token MATCH
%token WITH
%token <int> NAT
%token <string> TVAR
%token <string> VAR
%token <string> CTOR

%start <program> program
%start <typedef list> stdlib
%type <base_type> base_type
%type <value> value_nogroup
%type <value> value
%type <pat> pat
%type <expr> expr
%type <iso> iso_nogroup
%type <iso> iso
%type <term> term_nogroup
%type <term> term
%type <variant> variant
%type <typedef> typedef
%type <value * expr> biarrowed
%type <string> param
%%

wtf(separator, X):
  | x = X; separator; y = X; { [x; y] }
  | x = X; separator; xs = wtf(separator, X); { x :: xs }

program:
  | ts = typedef*; t = term; EOF; { { ts; t } }

stdlib:
  | ts = typedef*; EOF; { ts }

typedef:
  | TYPE; t = VAR; EQUAL; PIPE?; vs = separated_nonempty_list(PIPE, variant);
    { { vars = []; t; vs } }

  | TYPE; var = TVAR; t = VAR; EQUAL; PIPE?; vs = separated_nonempty_list(PIPE, variant);
    { { vars = [var]; t; vs } }

  | TYPE; LPAREN; vars = wtf(COMMA, TVAR); RPAREN; t = VAR; EQUAL; PIPE?; vs = separated_nonempty_list(PIPE, variant);
    { { vars; t; vs } }

variant:
  | c = CTOR; OF; a = base_type; { Iso { c; a } }
  | c = CTOR; { Value c }

base_type:
  | UNIT; { Unit }
  | ts = wtf(TIMES, base_type); { Product ts }
  | x = VAR; { Named x }
  | x = TVAR; { Var x }
  | t = base_type; a = VAR; { Ctor ([t], a) }
  | LPAREN; t = base_type; RPAREN; { t }
  | LPAREN; ts = wtf(COMMA, base_type); RPAREN; a = VAR; { Ctor (ts, a) }

value_nogroup:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; v = value; RPAREN; { v }
  | LPAREN; vs = wtf(COMMA, value); RPAREN; { Tuple vs }
  | x = VAR; { Var x }
  | x = CTOR; { Ctor x }
  | n = NAT; { nat_of_int n }
  | LBRACKET; RBRACKET; { Ctor "Nil" }
  | LBRACKET; vs = separated_nonempty_list(SEMICOLON, value); RBRACKET;
    {
      let f value acc = Cted { c = "Cons"; v = Tuple [value; acc] } in
      List.fold_right f vs (Ctor "Nil")
    }

value:
  | c = CTOR; v = value_nogroup; { Cted { c ; v } }

  (* weak r-associativity *)
  | v_1 = value; CONS; v_2 = value; { Cted { c = "Cons"; v = Tuple [v_1; v_2] } }
  | v = value_nogroup; { v }

pat:
  | x = VAR; { Named x }
  | LPAREN; ps = wtf(COMMA, pat); RPAREN; { Tuple ps }

expr:
  | v = value; { Value v }
  | LET; p_1 = pat; EQUAL; omega = iso; p_2 = pat; IN; e = expr; { Let { p_1; omega; p_2; e } }
  | LET; p_1 = pat; EQUAL; p_2_l = pat; CONS; p_2_r = pat; IN; e = expr;
    { Let { p_1; omega = Ctor "Cons"; p_2 = Tuple [p_2_l; p_2_r]; e } }

  | LET; p_1 = pat; EQUAL; MATCH; p_2 = pat; WITH;
    PIPE?; p = separated_nonempty_list(PIPE, biarrowed); IN; e = expr;
    { Let { p_1; omega = Pairs p; p_2; e } }

biarrowed:
  | v = value; BIARROW; e = expr; { (v, e) }

param:
  | phi = VAR; { phi }

iso_nogroup:
  | LBRACE; omega = iso; RBRACE; { omega }
  | x = VAR; { Var x }
  | x = CTOR; { Ctor x }

iso:
  | INVERT; omega = iso_nogroup; { Invert omega }
  | CASE; PIPE?; p = separated_nonempty_list(PIPE, biarrowed); { Pairs p }
  | FIX; phi = VAR; DOT; omega = iso; { Fix { phi; omega } }
  | FUN; params = param+; ARROW; omega = iso; { lambdas_of_params params omega }
  | omega_1 = iso; omega_2 = iso_nogroup; { App { omega_1; omega_2 } }
  | omega = iso_nogroup; { omega }

term_nogroup:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; t = term; RPAREN; { t }
  | LPAREN; ts = wtf(COMMA, term); RPAREN; { Tuple ts }
  | x = VAR; { Named x }
  | x = CTOR; { Named x }
  | LBRACKET; RBRACKET; { Named "Nil" }
  | LBRACKET; ts = separated_nonempty_list(SEMICOLON, term); RBRACKET;
    {
      let f t acc = App { omega = Ctor "Cons"; t = Tuple [t; acc] } in
      List.fold_right f ts (Named "Nil")
    }

  | n = NAT; { nat_of_int n |> term_of_value }

term:
  | omega = iso; t = term_nogroup; { App { omega; t } }
  | MATCH; t = term; WITH; PIPE?; p = separated_nonempty_list(PIPE, biarrowed);
    { App { omega = Pairs p; t } }

  | LET; p = pat; EQUAL; t_1 = term; IN; t_2 = term; { Let { p; t_1; t_2 } }
  | ISO; phi = VAR; params = param*; EQUAL; omega = iso; IN; t = term;
    { LetIso { phi; omega = lambdas_of_params params omega; t } }

  | ISO; REC; phi = VAR; params = param*; EQUAL; omega = iso; IN; t = term;
    {
      let omega = lambdas_of_params params omega in
      LetIso { phi; omega = Fix { phi; omega }; t }
    }

  (* weak r-associativity *)
  | t_1 = term; CONS; t_2 = term; { App { omega = Ctor "Cons"; t = Tuple [t_1; t_2] } }
  | t = term_nogroup; { t }
