module StrMap = Map.Make (String)

let ( let* ) : 'a option -> ('a -> 'b option) -> 'b option = Option.bind
let ( let+ ) (x : 'a option) (f : 'a -> 'b) = Option.map f x

let rec bind_all : 'a option list -> 'a list option = function
  | None :: _ -> None
  | Some x :: tl ->
      let+ tl = bind_all tl in
      x :: tl
  | [] -> Some []

let rec combine (l : 'a list) (r : 'b list) : ('a * 'b) list option =
  match (l, r) with
  | hdl :: tll, hdr :: tlr ->
      let+ tl = combine tll tlr in
      (hdl, hdr) :: tl
  | [], [] -> Some []
  | _ -> None

let extend (what : 'a StrMap.t) (list : (string * 'a) list) : 'a StrMap.t =
  let folder acc (key, value) = StrMap.add key value acc in
  List.fold_left folder what list

let value_or (value : 'a) : 'a option -> 'a = function
  | None -> value
  | Some value -> value

let is_variable (value : string) : bool =
  0 < String.length value
  &&
  let first = String.get value 0 |> Char.code in
  Char.code 'a' <= first && first <= Char.code 'z'
