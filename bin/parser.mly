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
%token FUNCTION
%token FUN
%token MATCH
%token WITH
%token <int> NAT
%token <string> TVAR
%token <string> VAR
%token <string> CTOR

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
%type <string> param
%%

wtf(separator, X):
  | x = X; separator; y = X; { [x; y] }
  | x = X; separator; xs = wtf(separator, X); { x :: xs }

program:
  | ts = typedef*; t = term; EOF; { { ts; t } }

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

value:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; v = value; RPAREN; { v }
  | LPAREN; vs = wtf(COMMA, value); RPAREN; { Tuple vs }
  | c = CTOR; v = value; { Cted { c ; v } }
  | x = VAR; { Named x }
  | x = CTOR; { Named x }
  | n = NAT; { nat_of_int n }
  | LBRACKET; RBRACKET; { Named "Nil" }
  | v_1 = value; CONS; v_2 = value; { Cted { c = "Cons"; v = Tuple [v_1; v_2] } }
  | LBRACKET; vs = separated_nonempty_list(SEMICOLON, value); RBRACKET;
    {
      let f value acc = Cted { c = "Cons"; v = Tuple [value; acc] } in
      List.fold_right f vs (Named "Nil")
    }

pat:
  | x = VAR; { Named x }
  | LPAREN; ps = wtf(COMMA, pat); RPAREN; { Tuple ps }

expr:
  | v = value; { Value v }
  | LET; p_1 = pat; EQUAL; omega = iso; p_2 = pat; IN; e = expr; { Let { p_1; omega; p_2; e } }
  | LET; p_1 = pat; EQUAL; MATCH; p_2 = pat; WITH;
    PIPE?; p = separated_nonempty_list(PIPE, biarrowed); IN; e = expr;
    {
      Let { p_1; omega = Pairs p; p_2; e }
    }

biarrowed:
  | v = value; BIARROW; e = expr; { (v, e) }

param:
  | phi = VAR; { phi }

iso:
  | LBRACE; omega = iso; RBRACE; { omega }
  | FUNCTION; PIPE?; p = separated_nonempty_list(PIPE, biarrowed); { Pairs p }
  | FIX; phi = VAR; DOT; omega = iso; { Fix { phi; omega } }
  | FUN; params = param+; ARROW; omega = iso; { lambdas_of_params params omega }
  | x = VAR; { Named x }
  | x = CTOR; { Named x }
  | omega_1 = iso; omega_2 = iso; { App { omega_1; omega_2 } }
  | INVERT; omega = iso; { Invert omega }

term:
  | LPAREN; RPAREN; { Unit }
  | LPAREN; t = term; RPAREN; { t }
  | LPAREN; ts = wtf(COMMA, term); RPAREN; { Tuple ts }
  | x = VAR; { Named x }
  | x = CTOR; { Named x }
  | omega = iso; t = term; { App { omega; t } }
  | MATCH; t = term; WITH; PIPE?; p = separated_nonempty_list(PIPE, biarrowed);
    { App { omega = Pairs p; t } }

  | LET; p = pat; EQUAL; t_1 = term; IN; t_2 = term; { Let { p; t_1; t_2 } }
  | LET; ISO; phi = VAR; params = param*; EQUAL; omega = iso; IN; t = term;
    { LetIso { phi; omega = lambdas_of_params params omega; t } }

  | LET; REC; phi = VAR; params = param*; EQUAL; omega = iso; IN; t = term;
    {
      let omega = Fix { phi; omega } in
      LetIso { phi; omega = lambdas_of_params params omega; t }
    }

  | LBRACKET; RBRACKET; { Named "Nil" }
  | t_1 = term; CONS; t_2 = term; { App { omega = Named "Cons"; t = Tuple [t_1; t_2] } }
  | LBRACKET; ts = separated_nonempty_list(SEMICOLON, term); RBRACKET;
    {
      let f t acc = App { omega = Named "Cons"; t = Tuple [t; acc] } in
      List.fold_right f ts (Named "Nil")
    }

  | n = NAT; { nat_of_int n |> term_of_value }
