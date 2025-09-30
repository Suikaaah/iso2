open Types

val invert : iso -> iso
val unify : pat -> term -> (string * term) list option
val subst : from:string -> into:term -> what:term -> term
val subst_iso : from:string -> into:iso -> what:iso -> iso
val subst_iso_in_expr : from:string -> into:iso -> what:expr -> expr
val value_of_term : term -> value option
val match_pair : (value * expr) list -> value -> (value * expr) option
val unify_value : value -> value -> (string * value) list
val eval : term -> term
val eval_iso : iso -> iso
