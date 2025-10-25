open Util

type base_type =
  | Unit
  | Product of base_type list
  | Named of string
  | Var of string
  | Ctor of base_type list * string

type iso_type =
  | BiArrow of { a : base_type; b : base_type }
  | Arrow of { t_1 : iso_type; t_2 : iso_type }
  | Var of int

type value =
  | Unit
  | Var of string
  | Ctor of string
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
  | Var of string
  | Ctor of string
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
type typedef = { vars : string list; t : string; vs : variant list }
type program = { ts : typedef list; t : term }

let rec term_of_value : value -> term = function
  | Unit -> Unit
  | Var x | Ctor x -> Named x
  | Cted { c; v } -> App { omega = Ctor c; t = term_of_value v }
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
  | Var x -> x = what
  | Ctor _ -> false
  | Cted { v; _ } -> contains_value what v
  | Tuple l -> List.exists (contains_value what) l

let contains_pairs (what : string) (pairs : (value * expr) list) : bool =
  List.exists (fun (v, _) -> contains_value what v) pairs

let rec lambdas_of_params : string list -> iso -> iso = function
  | [] -> fun omega -> omega
  | psi :: tl -> fun omega -> Lambda { psi; omega = lambdas_of_params tl omega }

let rec is_list_value : value -> bool = function
  | Cted { c = "Cons"; v = Tuple [ _; v ] } -> is_list_value v
  | Ctor "Nil" -> true
  | _ -> false

let rec is_list_term : term -> bool = function
  | App { omega = Ctor "Cons"; t = Tuple [ _; t ] } -> is_list_term t
  | Named "Nil" -> true
  | _ -> false

let rec is_int_value : value -> bool = function
  | Cted { c = "S"; v } -> is_int_value v
  | Ctor "Z" -> true
  | _ -> false

let rec is_int_term : term -> bool = function
  | App { omega = Ctor "S"; t } -> is_int_term t
  | Named "Z" -> true
  | _ -> false

let rec show_base_type : base_type -> string = function
  | Unit -> "unit"
  | Product l ->
      show_listlike show_base_type ~left:"(" ~delim:" * " ~right:")" l
  | Named x | Var x -> x
  | Ctor ([], _) -> "unreachable (type constructor with Z arity)"
  | Ctor ([ x ], a) -> show_base_type x ^ " " ^ a
  | Ctor (l, a) -> show_tuple show_base_type l ^ " " ^ a

let rec show_iso_type : iso_type -> string = function
  | BiArrow { a; b } -> show_base_type a ^ " <-> " ^ show_base_type b
  | Arrow { t_1 = Var _ as t_1; t_2 = BiArrow _ as t_2 } ->
      show_iso_type t_1 ^ " -> (" ^ show_iso_type t_2 ^ ")"
  | Arrow { t_1 = Var _ as t_1; t_2 } ->
      show_iso_type t_1 ^ " -> " ^ show_iso_type t_2
  | Arrow { t_1; t_2 = BiArrow _ as t_2 } ->
      "(" ^ show_iso_type t_1 ^ ") -> (" ^ show_iso_type t_2 ^ ")"
  | Arrow { t_1; t_2 } -> "(" ^ show_iso_type t_1 ^ ") -> " ^ show_iso_type t_2
  | Var x -> "'" ^ string_of_int x

let rec show_value : value -> string = function
  | Unit -> "()"
  | Ctor "Z" -> "0"
  | Ctor "Nil" -> "[]"
  | Ctor x | Var x -> x
  | Cted { c = "S"; v } -> begin
      let rec lmao acc = function
        | Cted { c = "S"; v } -> lmao (acc + 1) v
        | Ctor "Z" -> Ok acc
        | otherwise -> Error (repeat "S " acc ^ show_value otherwise)
      in
      match lmao 1 v with Ok n -> string_of_int n | Error t -> t
    end
  | Cted { c = "Cons"; v = Tuple [ v_1; v_2 ] } as v ->
      if is_list_value v then
        let rec lmao = function
          | Cted { c = "Cons"; v = Tuple [ v_1; v_2 ] } ->
              "; " ^ show_value v_1 ^ lmao v_2
          | Ctor "Nil" -> ""
          | otherwise -> "; " ^ show_value otherwise
        in
        "[" ^ show_value v_1 ^ lmao v_2 ^ "]"
      else
        let rec lmao = function
          | Cted { c = "Cons"; v = Tuple [ v_1; v_2 ] } ->
              " :: " ^ show_value v_1 ^ lmao v_2
          | otherwise -> " :: " ^ show_value otherwise
        in
        show_value v_1 ^ lmao v_2
  | Tuple l -> show_tuple show_value l
  | Cted { c; v = Cted _ as v } when not (is_int_value v || is_list_value v) ->
      c ^ " (" ^ show_value v ^ ")"
  | Cted { c; v } -> c ^ " " ^ show_value v

let rec show_pat : pat -> string = function
  | Named x -> x
  | Tuple l -> show_tuple show_pat l

let rec show_expr : expr -> string = function
  | Value v -> show_value v
  | Let { p_1; omega = (Ctor _ | Var _) as omega; p_2; e } ->
      "let " ^ show_pat p_1 ^ " = " ^ show_iso omega ^ " " ^ show_pat p_2
      ^ " in " ^ show_expr e
  | Let { p_1; omega; p_2; e } ->
      "let " ^ show_pat p_1 ^ " = {" ^ show_iso omega ^ "} " ^ show_pat p_2
      ^ " in " ^ show_expr e

and show_pairs (pairs : (value * expr) list) : string =
  List.fold_left
    (fun acc (v, e) -> acc ^ "\n  | " ^ show_value v ^ " <-> " ^ show_expr e)
    "function" pairs

and show_iso : iso -> string = function
  | Pairs p -> show_pairs p
  | Fix { phi; omega; _ } -> "fix " ^ phi ^ ". " ^ show_iso omega
  | Lambda { psi; omega; _ } -> "fun " ^ psi ^ " -> " ^ show_iso omega
  | Ctor omega | Var omega -> omega
  | App { omega_1; omega_2 = (Ctor _ | Var _) as omega_2 } ->
      show_iso omega_1 ^ " " ^ show_iso omega_2
  | App { omega_1; omega_2 } -> show_iso omega_1 ^ " {" ^ show_iso omega_2 ^ "}"
  | Invert ((Ctor _ | Var _) as omega) -> "invert " ^ show_iso omega
  | Invert omega -> "invert {" ^ show_iso omega ^ "}"

let show_pairs_lhs (pairs : (value * expr) list) : string =
  List.fold_left
    (fun acc (v, _) -> acc ^ "\n  | " ^ show_value v ^ " <-> ...")
    "function" pairs

let rec show_term : term -> string = function
  | Unit -> "()"
  | Named "Z" -> "0"
  | Named "Nil" -> "[]"
  | Named x -> x
  | Tuple l -> show_tuple show_term l
  | App { omega = Ctor "S"; t } -> begin
      let rec lmao acc = function
        | App { omega = Ctor "S"; t } -> lmao (acc + 1) t
        | Named "Z" -> Ok acc
        | otherwise -> Error (repeat "S " acc ^ show_term otherwise)
      in
      match lmao 1 t with Ok n -> string_of_int n | Error t -> t
    end
  | App { omega = Ctor "Cons"; t = Tuple [ t_1; t_2 ] } as t ->
      if is_list_term t then
        let rec lmao = function
          | App { omega = Ctor "Cons"; t = Tuple [ t_1; t_2 ] } ->
              "; " ^ show_term t_1 ^ lmao t_2
          | Named "Nil" -> ""
          | otherwise -> "; " ^ show_term otherwise
        in
        "[" ^ show_term t_1 ^ lmao t_2 ^ "]"
      else
        let rec lmao = function
          | App { omega = Ctor "Cons"; t = Tuple [ t_1; t_2 ] } ->
              " :: " ^ show_term t_1 ^ lmao t_2
          | otherwise -> " :: " ^ show_term otherwise
        in
        show_term t_1 ^ lmao t_2
  | App { omega; t = App _ as t } when not (is_int_term t || is_list_term t) ->
      show_iso omega ^ " (" ^ show_term t ^ ")"
  | App { omega; t = (Let _ | LetIso _) as t } ->
      show_iso omega ^ " (" ^ show_term t ^ ")"
  | App { omega; t } -> show_iso omega ^ " " ^ show_term t
  | Let { p; t_1; t_2 } ->
      "let " ^ show_pat p ^ " = " ^ show_term t_1 ^ " in\n" ^ show_term t_2
  | LetIso { phi; omega; t } ->
      "let iso " ^ phi ^ " = " ^ show_iso omega ^ " in\n" ^ show_term t

let rec nat_of_int (n : int) : value =
  if n < 1 then Ctor "Z" else Cted { c = "S"; v = nat_of_int (n - 1) }

let rec build_storage (default : 'a) : value -> 'a option StrMap.t = function
  | Unit -> StrMap.empty
  | Var x -> StrMap.singleton x None
  | Ctor _ -> StrMap.empty
  | Cted { v; _ } -> build_storage default v
  | Tuple l ->
      List.map (build_storage default) l
      |> List.fold_left
           (StrMap.union (fun _ _ _ -> Some (Some default)))
           StrMap.empty
