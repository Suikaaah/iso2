open Types
open Util

type equation = value * value
type subst = { what : string; into : value }

let rec subst (s : subst) : value -> value = function
  | Unit -> Unit
  | Named x when x = s.what && is_variable x -> s.into
  | Named x -> Named x
  | Cted { c; v } -> Cted { c; v = subst s v }
  | Tuple l -> Tuple (List.map (subst s) l)

let subst_in_equations (s : subst) : equation list -> equation list =
  List.map (fun (a, b) -> (subst s a, subst s b))

let rec occurs (x : string) : value -> bool = function
  | Unit -> false
  | Named y when is_variable y -> x = y
  | Named _ -> false
  | Cted { v; _ } -> occurs x v
  | Tuple l -> List.exists (occurs x) l

let is_free (x : string) (v : value) : bool = not (occurs x v)

let rec unify : equation list -> (subst list, unit) result = function
  | [] -> Ok []
  | e :: e' -> begin
      match e with
      | a, b when a = b -> unify e'
      | Named x, b when is_variable x && is_free x b ->
          let s = { what = x; into = b } in
          let++ unified = subst_in_equations s e' |> unify in
          s :: unified
      | a, Named x when is_variable x && is_free x a ->
          let s = { what = x; into = a } in
          let++ unified = subst_in_equations s e' |> unify in
          s :: unified
      | Cted { c = c_1; v = v_1 }, Cted { c = c_2; v = v_2 } when c_1 = c_2 ->
          (v_1, v_2) :: e' |> unify
      | Tuple a, Tuple b when List.compare_lengths a b = 0 ->
          List.combine a b @ e' |> unify
      | _ -> Error ()
    end

let rec collect_vars : value -> string list = function
  | Unit -> []
  | Named x when is_variable x -> [ x ]
  | Named _ -> []
  | Cted { v; _ } -> collect_vars v
  | Tuple l -> List.map collect_vars l |> List.flatten

let is_orthogonal (u : value) (v : value) : unit myresult =
  let gen = ref 0 in
  let fresh_name () =
    let name : value = Named ("x_" ^ string_of_int !gen) in
    incr gen;
    name
  in

  let convert v =
    let vars = collect_vars v |> StrSet.of_list in
    let substs =
      StrSet.fold
        (fun x substs -> { what = x; into = fresh_name () } :: substs)
        vars []
    in
    List.fold_left (fun v s -> subst s v) v substs
  in

  let u' = convert u in
  let v' = convert v in

  match unify [ (u', v') ] with
  | Ok l ->
      let msg =
        show_value u ^ " and " ^ show_value v ^ " are not orthogonal\n"
        ^ "example: "
        ^ show_list (fun { what; into } -> what ^ " = " ^ show_value into) l
        ^ "\nwhere " ^ show_value u ^ " = " ^ show_value u' ^ " and "
        ^ show_value v ^ " = " ^ show_value v'
      in
      Error msg
  | Error () -> Ok ()
