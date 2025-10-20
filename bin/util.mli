module StrMap : module type of Map.Make (String)

type 'a myresult = ('a, string) Result.t

val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
val ( let** ) : 'a myresult -> ('a -> 'b myresult) -> 'b myresult
val ( let++ ) : 'a myresult -> ('a -> 'b) -> 'b myresult
val bind_all : 'a myresult list -> 'a list myresult
val combine : 'a list -> 'b list -> ('a * 'b) list option
val extend : 'a StrMap.t -> (string * 'a) list -> 'a StrMap.t
val is_variable : string -> bool
val is_type_variable : string -> bool
val for_all_pairs : ('a -> 'a -> 'b option) -> 'a list -> 'b option
val union_nodup : 'a StrMap.t -> 'a StrMap.t -> 'a StrMap.t myresult
val union : weak:'a StrMap.t -> strong:'a StrMap.t -> 'a StrMap.t
val union_list : 'a StrMap.t list -> 'a StrMap.t
val show_tuple : ('a -> string) -> 'a -> 'a list -> string
val show_list : ('a -> string) -> 'a -> 'a list -> string
val boldred : string -> string
val find_res : string -> 'a StrMap.t -> 'a myresult
