module StrMap = Map.Make (String)

type 'a myresult = ('a, string) Result.t

let ( let* ) : 'a option -> ('a -> 'b option) -> 'b option = Option.bind
let ( let+ ) (x : 'a option) (f : 'a -> 'b) : 'b option = Option.map f x
let ( let** ) : 'a myresult -> ('a -> 'b myresult) -> 'b myresult = Result.bind
let ( let++ ) (x : 'a myresult) (f : 'a -> 'b) : 'b myresult = Result.map f x

let rec bind_all : 'a myresult list -> 'a list myresult = function
  | Error e :: _ -> Error e
  | Ok x :: tl ->
      let++ tl = bind_all tl in
      x :: tl
  | [] -> Ok []

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

let is_variable (value : string) : bool =
  0 < String.length value
  &&
  let first = String.get value 0 |> Char.code in
  Char.code 'a' <= first && first <= Char.code 'z'

let rec for_all_pairs (f : 'a -> 'a -> 'b option) : 'a list -> 'b option =
  function
  | [] -> None
  | hd :: tl -> begin
      match List.find_map (f hd) tl with
      | Some x -> Some x
      | None -> for_all_pairs f tl
    end

let union_nodup (l : 'a StrMap.t) (r : 'a StrMap.t) : 'a StrMap.t myresult =
  let msg = ref None in
  let merger key _ y =
    msg := Some (key ^ " already exists");
    Some y
  in
  let merged = StrMap.union merger l r in
  match !msg with None -> Ok merged | Some msg -> Error msg

let union ~(weak : 'a StrMap.t) ~(strong : 'a StrMap.t) : 'a StrMap.t =
  let merger _ _ y = Some y in
  StrMap.union merger weak strong

let show_list (f : 'a -> string) (hd : 'a) (tl : 'a list) : string =
  let init = "(" ^ f hd in
  List.fold_left (fun acc a -> acc ^ ", " ^ f a) init tl ^ ")"

let boldred (value : string) : string = "\x1b[1;31m" ^ value ^ "\x1b[0m"
