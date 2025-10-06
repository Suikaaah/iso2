module StrMap : module type of Map.Make (String)

type 'a myresult = Ok of 'a | Err of string

val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
val bind_all : 'a option list -> 'a list option
val combine : 'a list -> 'b list -> ('a * 'b) list option
val extend : 'a StrMap.t -> (string * 'a) list -> 'a StrMap.t
val value_or : 'a -> 'a option -> 'a
val is_variable : string -> bool
val for_all_pairs : ('a -> 'a -> bool) -> 'a list -> bool
val union_nodup : 'a StrMap.t -> 'a StrMap.t -> 'a StrMap.t
val union : weak:'a StrMap.t -> strong:'a StrMap.t -> 'a StrMap.t
