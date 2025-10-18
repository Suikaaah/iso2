type base_type =
  | Unit
  | Product of base_type list
  | Named of string
  | Var of int

type iso_type =
  | BiArrow of { a : base_type; b : base_type }
  | Arrow of { t_1 : iso_type; t_2 : iso_type }

type value =
  | Unit
  | Named of string
  | Cted of { c : string; v : value }
  | Tuple of value list

type pat = Named of string | Tuple of pat list

type expr =
  | Value of value
  | Let of { p_1 : pat; omega : iso; p_2 : pat; e : expr }

and iso =
  | Pairs of (value * expr) list
  | Fix of { phi : string; omega : iso }
  | Lambda of { psi : string; omega : iso }
  | Named of string
  | App of { omega_1 : iso; omega_2 : iso }
  | Invert of iso

type term =
  | Unit
  | Named of string
  | Tuple of term list
  | App of { omega : iso; t : term }
  | Let of { p : pat; t_1 : term; t_2 : term }
  | LetIso of { phi : string; omega : iso; t : term }

type variant = Value of string | Iso of { c : string; a : base_type }
type typedef = { t : string; vs : variant list }
type program = { ts : typedef list; t : term }

val term_of_value : value -> term
val term_of_pat : pat -> term
val term_of_expr : expr -> term
val value_of_expr : expr -> value
val contains : string -> pat -> bool
val contains_value : string -> value -> bool
val contains_pairs : string -> (value * expr) list -> bool
val lambdas_of_params : string list -> iso -> iso
val show_base_type : base_type -> string
val show_iso_type : iso_type -> string
val show_value : value -> string
val show_pat : pat -> string
val show_expr : expr -> string
val show_pairs : (value * expr) list -> string
val show_iso : iso -> string
val show_term : term -> string
