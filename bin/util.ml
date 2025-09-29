module StrMap = Map.Make (String)

let ( let* ) = Option.bind
let ( let+ ) x f = Option.map f x

let rec bind_all = function
  | None :: _ -> None
  | Some x :: tl ->
      let+ tl = bind_all tl in
      x :: tl
  | [] -> Some []

let rec combine l r =
  match (l, r) with
  | hdl :: tll, hdr :: tlr ->
      let+ tl = combine tll tlr in
      (hdl, hdr) :: tl
  | [], [] -> Some []
  | _ -> None

let extend what list =
  let folder acc (key, value) = StrMap.add key value acc in
  List.fold_left folder what list
