type base_type =
  | Unit
  | Sum of base_type list
  | Product of base_type list
  | Named of string
[@@deriving show { with_path = false }]

type iso_type =
  | BiArrow of { a : base_type; b : base_type }
  | Arrow of { t_1 : iso_type; t_2 : iso_type }
[@@deriving show { with_path = false }]

type value =
  | Unit
  | Named of string
  | Cted of { c : string; v : value }
  | Tuple of value list
[@@deriving show { with_path = false }]

type pat = Named of string | Tuple of pat list
[@@deriving show { with_path = false }]

type expr =
  | Value of value
  | Let of { p_1 : pat; omega : iso; p_2 : pat; e : expr }
[@@deriving show { with_path = false }]

and iso =
  | Pairs of { anot : iso_type; pairs : (value * expr) list }
  | Fix of { phi : string; anot : iso_type; omega : iso }
  | Lambda of { psi : string; anot : iso_type; omega : iso }
  | Named of string
  | App of { omega_1 : iso; omega_2 : iso }
  | Invert of iso
[@@deriving show { with_path = false }]

type term =
  | Unit
  | Named of string
  | Tuple of term list
  | App of { omega : iso; t : term }
  | Let of { p : pat; t_1 : term; t_2 : term }
  | LetIso of { phi : string; omega : iso; t : term }
[@@deriving show { with_path = false }]

type variant = Value of string | Iso of { c : string; a : base_type }
[@@deriving show { with_path = false }]

type typedef = { t : string; vs : variant list }
[@@deriving show { with_path = false }]

type program = { ts : typedef list; t : term }
[@@deriving show { with_path = false }]

let rec term_of_value : value -> term = function
  | Unit -> Unit
  | Named x -> Named x
  | Cted { c; v } -> App { omega = Named c; t = term_of_value v }
  | Tuple l -> Tuple (List.map term_of_value l)

let rec term_of_pat : pat -> term = function
  | Named x -> Named x
  | Tuple l -> Tuple (List.map term_of_pat l)

let rec term_of_expr : expr -> term = function
  | Value v -> term_of_value v
  | Let { p_1; omega; p_2; e } ->
      Let
        {
          p = p_1;
          t_1 = App { omega; t = term_of_pat p_2 };
          t_2 = term_of_expr e;
        }

let rec value_of_expr : expr -> value = function
  | Value v -> v
  | Let { e; _ } -> value_of_expr e

let rec p_term : term -> string = function
  | Unit -> "()"
  | Named x -> x
  | Tuple (hd :: tl) ->
      let init = "(" ^ p_term hd in
      List.fold_left (fun acc x -> acc ^ ", " ^ p_term x) init tl ^ ")"
  | App { omega = Named omega; t } -> omega ^ " " ^ p_term t
  | _ -> "<bruh>"

let rec contains (what : string) : pat -> bool = function
  | Named x -> x = what
  | Tuple l -> List.exists (contains what) l

let rec contains_value (what : string) : value -> bool = function
  | Unit -> false
  | Named x -> x = what
  | Cted { v; _ } -> contains_value what v
  | Tuple l -> List.exists (contains_value what) l

let contains_pairs (what : string) (pairs : (value * expr) list) : bool =
  List.exists (fun (v, _) -> contains_value what v) pairs
