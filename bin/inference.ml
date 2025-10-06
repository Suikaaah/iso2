open Types
open Util

type psi = iso_type StrMap.t
type delta = base_type StrMap.t
type context = { psi : psi; delta : delta }

let rec is_orthogonal (u : value) (v : value) : string option =
  let msg =
    lazy (Some (p_value u ^ " and " ^ p_value v ^ " are not orthogonal"))
  in
  match (u, v) with
  | Unit, Unit -> Lazy.force msg
  | Named x, _ when is_variable x -> Lazy.force msg
  | _, Named x when is_variable x -> Lazy.force msg
  | Named x, Named y when x = y -> Lazy.force msg
  | Cted { c = c_1; v = v_1 }, Cted { c = c_2; v = v_2 } ->
      if c_1 = c_2 then is_orthogonal v_1 v_2 else None
  | Tuple l, Tuple r ->
      let mapped =
        combine l r |> Option.get |> List.map (fun (u, v) -> is_orthogonal u v)
      in
      let is_error = List.for_all Option.is_some mapped in
      if is_error then Lazy.force msg else None
  | _ -> None

let rec invert_iso_type : iso_type -> iso_type = function
  | BiArrow { a; b } -> BiArrow { a = b; b = a }
  | Arrow { t_1; t_2 } ->
      let t_1 = invert_iso_type t_1 in
      let t_2 = invert_iso_type t_2 in
      Arrow { t_1; t_2 }

let rec unify_pat (p : pat) (a : base_type) : delta option =
  match (p, a) with
  | Named x, _ -> Some (StrMap.singleton x a)
  | Tuple t, Product p ->
      let* combined = combine t p in
      let+ list = List.map (fun (p, a) -> unify_pat p a) combined |> bind_all in
      List.fold_left (fun acc delta -> union_nodup acc delta) StrMap.empty list
  | _ -> None

let rec unify_value (ctx : context) (v : value) (a : base_type) : delta option =
  match (v, a) with
  | Unit, Unit -> Some StrMap.empty
  | Named x, _ when is_variable x -> Some (StrMap.singleton x a)
  | Named x, _ ->
      let* b = StrMap.find_opt x ctx.delta in
      if a = b then Some StrMap.empty else None
  | Cted { c; v }, b' -> begin
      let* c = StrMap.find_opt c ctx.psi in
      match c with
      | BiArrow { a; b } when b = b' -> unify_value ctx v a
      | _ -> None
    end
  | Tuple t, Product p ->
      let* combined = combine t p in
      let+ list =
        List.map (fun (v, a) -> unify_value ctx v a) combined |> bind_all
      in
      List.fold_left (fun acc delta -> union_nodup acc delta) StrMap.empty list
  | _ -> None

let invert_pairs (pairs : (value * expr) list) : (value * expr) list =
  let rec invert_expr (e : expr) (acc : expr) =
    match e with
    | Value v -> (v, acc)
    | Let { p_1; omega; p_2; e } ->
        invert_expr e
          (Let { p_1 = p_2; omega = Invert omega; p_2 = p_1; e = acc })
  in
  let invert_pair (v, e) = invert_expr e (Value v) in
  List.map invert_pair pairs

let rec infer_base_in_expr (ctx : context) (e : expr) : base_type option =
  match e with
  | Value v -> infer_base ctx (term_of_value v)
  | Let { p_1; omega; e; _ } ->
      let* omega = infer_iso ctx omega in
      let* b = match omega with BiArrow { b; _ } -> Some b | _ -> None in
      let* unified = unify_pat p_1 b in
      let extended = union ~weak:ctx.delta ~strong:unified in
      infer_base_in_expr { psi = ctx.psi; delta = extended } e

and infer_base (ctx : context) (t : term) : base_type option =
  match t with
  | Unit -> Some Unit
  | Named x -> StrMap.find_opt x ctx.delta
  | Tuple l ->
      let+ l = List.map (infer_base ctx) l |> bind_all in
      Product l
  | App { omega; t } -> begin
      let* omega = infer_iso ctx omega in
      let* t = infer_base ctx t in
      match omega with BiArrow { a; b } when a = t -> Some b | _ -> None
    end
  | Let { p; t_1; t_2 } ->
      let* a = infer_base ctx t_1 in
      let* unified = unify_pat p a in
      let extended = union ~weak:ctx.delta ~strong:unified in
      infer_base { psi = ctx.psi; delta = extended } t_2
  | LetIso { phi; omega; t } ->
      let* omega = infer_iso ctx omega in
      let extended = StrMap.add phi omega ctx.psi in
      infer_base { psi = extended; delta = ctx.delta } t

and infer_iso (ctx : context) (omega : iso) : iso_type option =
  match omega with
  | Pairs { anot = BiArrow { a; b }; pairs } ->
      let infer_pairs a b pairs =
        let well_typed (v, e) =
          let infered =
            let* unified = unify_value ctx v a in
            let extended = union ~weak:ctx.delta ~strong:unified in
            infer_base_in_expr { psi = ctx.psi; delta = extended } e
          in
          match infered with Some b' when b' = b -> true | _ -> false
        in
        let pred u v =
          match is_orthogonal u v with
          | None -> true
          | Some msg ->
              print_endline msg;
              false
        in
        if List.for_all well_typed pairs then
          let is_orthogonal_v =
            List.map (fun (v, _) -> v) pairs |> for_all_pairs pred
          in
          let is_orthogonal_e =
            List.map (fun (_, e) -> value_of_expr e) pairs |> for_all_pairs pred
          in
          if is_orthogonal_e && is_orthogonal_v then Some (BiArrow { a; b })
          else None
        else None
      in
      let* _ = infer_pairs b a (invert_pairs pairs) in
      infer_pairs a b pairs
  | Pairs _ -> None
  | Fix { phi; anot; omega } ->
      let extended = extend ctx.psi [ (phi, anot) ] in
      let* omega = infer_iso { psi = extended; delta = ctx.delta } omega in
      if omega = anot then Some anot else None
  | Lambda { psi; anot; omega } ->
      let extended = extend ctx.psi [ (psi, anot) ] in
      let+ t_2 = infer_iso { psi = extended; delta = ctx.delta } omega in
      Arrow { t_1 = anot; t_2 }
  | Named x -> StrMap.find_opt x ctx.psi
  | App { omega_1; omega_2 } -> begin
      let* omega_1 = infer_iso ctx omega_1 in
      let* omega_2 = infer_iso ctx omega_2 in
      match omega_1 with
      | Arrow { t_1; t_2 } when t_1 = omega_2 -> Some t_2
      | _ -> None
    end
  | Invert omega -> Option.map invert_iso_type (infer_iso ctx omega)

let build_ctx (defs : typedef list) : context =
  let module Local = struct
    type lctx = {
      psi : (string * iso_type) list;
      delta : (string * base_type) list;
    }

    let empty_lctx = { psi = []; delta = [] }

    let append { psi = psi_1; delta = delta_1 } { psi = psi_2; delta = delta_2 }
        =
      { psi = psi_1 @ psi_2; delta = delta_1 @ delta_2 }
  end in
  let open Local in
  let build_lctx { t; vs } =
    let f { psi; delta } v =
      match v with
      | Value x -> { psi; delta = (x, Named t) :: delta }
      | Iso { c; a } -> { psi = (c, BiArrow { a; b = Named t }) :: psi; delta }
    in
    List.fold_left f empty_lctx vs
  in
  let { psi; delta } =
    List.fold_left (fun acc x -> append acc (build_lctx x)) empty_lctx defs
  in
  { psi = StrMap.of_list psi; delta = StrMap.of_list delta }
