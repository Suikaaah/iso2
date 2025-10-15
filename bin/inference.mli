open Util

type any =
  | Unit
  | Product of any list
  | Named of string
  | BiArrow of { a : any; b : any }
  | Arrow of { a : any; b : any }
  | Var of int

type equation = any * any
type subst = { what : int; into : any }
type infered = { a : any; e : equation list }
type elt = Mono of any | Scheme of { forall : int list; a : any }
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

val infer_term : Types.term -> generator -> context -> infered myresult
val infer_iso : Types.iso -> generator -> context -> infered myresult
val invert_iso_type : Types.iso_type -> Types.iso_type

(*
val is_orthogonal : value -> value -> string option
val unify_value : context -> value -> base_type -> delta myresult
val invert_pairs : (value * expr) list -> (value * expr) list
val unify_pat : pat -> base_type -> base_type StrMap.t myresult
val infer_base_in_expr : context -> expr -> base_type myresult
val infer_base : context -> term -> base_type myresult
val infer_iso : context -> iso -> iso_type myresult
val build_ctx : typedef list -> context
*)
