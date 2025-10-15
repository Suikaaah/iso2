open Types
open Util

type psi = iso_type StrMap.t

val invert : psi -> iso -> iso
val unify : pat -> term -> (string * term) list
val subst : from:string -> into:term -> what:term -> term
val subst_iso : from:string -> into:iso -> what:iso -> iso
val subst_iso_in_expr : from:string -> into:iso -> what:expr -> expr
val subst_iso_in_term : from:string -> into:iso -> what:term -> term
val value_of_term : term -> value
val match_pair : (value * expr) list -> value -> (value * expr) option
val unify_value : value -> value -> (string * value) list
val eval : psi -> term -> term
val eval_iso : psi -> iso -> iso
