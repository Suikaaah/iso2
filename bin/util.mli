module StrMap : module type of Map.Make (String)

val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
val bind_all : 'a option list -> 'a list option
val combine : 'a list -> 'b list -> ('a * 'b) list option
val extend : 'a StrMap.t -> (string * 'a) list -> 'a StrMap.t
