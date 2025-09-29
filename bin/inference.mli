open Types
open Util

type psi = iso_type StrMap.t
type delta = base_type StrMap.t
type context = { psi : psi; delta : delta }

val invert_iso_type : iso_type -> iso_type
val term_of_value : value -> term
val unify_pat : pat -> base_type -> (string * base_type) list option

val unify_value :
  context -> value -> base_type -> (string * base_type) list option

val infer_base_in_expr : context -> expr -> base_type option
val infer_base : context -> term -> base_type option
val infer_iso : context -> iso -> iso_type option
val build_ctx : typedef list -> context
