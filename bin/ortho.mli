open Types
open Util

type equation
type subst

val subst : subst -> value -> value
val subst_in_equations : subst -> equation list -> equation list
val occurs : string -> value -> bool
val is_free : string -> value -> bool
val unify : equation list -> (subst list, unit) result
val collect_vars : value -> string list
val is_orthogonal : value -> value -> unit myresult

