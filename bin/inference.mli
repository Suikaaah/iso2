open Util

type any =
  | Unit
  | Product of any list
  | Named of string
  | BiArrow of { a : any; b : any }
  | Arrow of { a : any; b : any }
  | Var of int
[@@deriving show]

type equation = any * any
type subst = { what : int; into : any }
type infered_pair = { a_v : any; a_e : any; e : equation list }
type infered = { a : any; e : equation list }

type elt = Mono of any | Scheme of { forall : int list; a : any }
[@@deriving show]

type context = elt StrMap.t
type generator = { mutable i : int }

val fresh : generator -> int
val subst : subst -> any -> any
val subst_in_context : subst -> context -> context
val subst_in_equations : subst -> equation list -> equation list
val instantiate : generator -> elt -> any
val occurs : int -> any -> bool
val unify : equation list -> subst list myresult

(* (a, (b, c)) -> ('0, ('1, '2)) and [(a, '0); (b, '1); (c, '2)] *)
val context_of_pat : generator -> Types.pat -> any * any StrMap.t
val find_generalizable : any -> context -> int list

val generalize :
  equation list ->
  context ->
  Types.pat ->
  any ->
  generator ->
  (context * equation) myresult

val extract_named : generator -> Types.value -> context
val invert_iso_type : any -> any myresult

val infer_pair :
  generator -> context -> Types.value * Types.expr -> infered_pair myresult

val infer_term : Types.term -> generator -> context -> infered myresult
val infer_expr : Types.expr -> generator -> context -> infered myresult
val infer_iso : Types.iso -> generator -> context -> infered myresult
val any_of_base : Types.base_type -> any
val base_of_any : any -> Types.base_type myresult
val iso_of_any : any -> Types.iso_type myresult
val build_ctx : generator -> Types.typedef list -> context
val finalize : infered -> any myresult

(*
val is_orthogonal : value -> value -> string option
val invert_pairs : (value * expr) list -> (value * expr) list
*)
