open Util

type base_type = Unit | Product of base_type list | Named of string

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
  | Pairs of { annot : iso_type; pairs : (value * expr) list }
  | Fix of { phi : string; annot : iso_type; omega : iso }
  | Lambda of { psi : string; annot : iso_type; omega : iso }
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

let rec lambdas_of_params : (string * iso_type) list -> iso -> iso = function
  | [] -> fun omega -> omega
  | (psi, annot) :: tl ->
      fun omega -> Lambda { psi; annot; omega = lambdas_of_params tl omega }

let rec show_base_type : base_type -> string = function
  | Unit -> "()"
  | Product (hd :: tl) -> show_list show_base_type hd tl
  | Product _ -> "unreachable"
  | Named x -> x

let rec show_iso_type : iso_type -> string = function
  | BiArrow { a; b } ->
      "(" ^ show_base_type a ^ " <-> " ^ show_base_type b ^ ")"
  | Arrow { t_1; t_2 } ->
      "(" ^ show_iso_type t_1 ^ " -> " ^ show_iso_type t_2 ^ ")"

let rec show_value : value -> string = function
  | Unit -> "()"
  | Named x -> x
  | Cted { c; v = Tuple [ v_1; v_2 ] } when is_infix c ->
      show_value v_1 ^ " " ^ c ^ " " ^ show_value v_2
  | Cted { c; v } -> c ^ " " ^ show_value v
  | Tuple (hd :: tl) -> show_list show_value hd tl
  | Tuple _ -> "unreachable"

let rec show_pat : pat -> string = function
  | Named x -> x
  | Tuple (hd :: tl) -> show_list show_pat hd tl
  | Tuple _ -> "unreachable"

let rec show_expr : expr -> string = function
  | Value v -> show_value v
  | Let { p_1; omega; p_2; e } ->
      "let " ^ show_pat p_1 ^ " = (" ^ show_iso omega ^ ") " ^ show_pat p_2
      ^ " in " ^ show_expr e

and show_pairs (pairs : (value * expr) list) : string =
  List.fold_left
    (fun acc (v, e) -> acc ^ "\n  " ^ show_value v ^ " <-> " ^ show_expr e)
    "{" pairs
  ^ "\n}"

and show_iso : iso -> string = function
  | Pairs { pairs; _ } -> show_pairs pairs
  | Fix { phi; omega; _ } -> "fix " ^ phi ^ ". " ^ show_iso omega
  | Lambda { psi; omega; _ } -> "\\" ^ psi ^ ". " ^ show_iso omega
  | Named omega -> omega
  | App { omega_1; omega_2 } -> "(" ^ show_iso omega_1 ^ ") " ^ show_iso omega_2
  | Invert omega -> "invert " ^ show_iso omega

let rec show_term : term -> string = function
  | Unit -> "()"
  | Named x -> x
  | Tuple (hd :: tl) -> show_list show_term hd tl
  | Tuple _ -> "unreachable"
  | App { omega; t } -> "(" ^ show_iso omega ^ ") " ^ show_term t
  | Let { p; t_1; t_2 } ->
      "let " ^ show_pat p ^ " = " ^ show_term t_1 ^ " in " ^ show_term t_2
  | LetIso { phi; omega; t } ->
      "let iso " ^ phi ^ " = (" ^ show_iso omega ^ ") " ^ show_term t
