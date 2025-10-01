open Types
open Util

let rec invert (omega : iso) : iso =
  let open Inference in
  match omega with
  | Pairs { anot; pairs } ->
      let rec invert_expr (e : expr) (acc : expr) =
        match e with
        | Value v -> (v, acc)
        | Let { p_1; omega; p_2; e } ->
            invert_expr e
              (Let { p_1 = p_2; omega = invert omega; p_2 = p_1; e = acc })
      in
      let invert_pair (v, e) = invert_expr e (Value v) in
      Pairs { anot = invert_iso_type anot; pairs = List.map invert_pair pairs }
  | Fix { phi; anot; omega } ->
      Fix { phi; anot = invert_iso_type anot; omega = invert omega }
  | Lambda { psi; anot; omega } ->
      Lambda { psi; anot = invert_iso_type anot; omega = invert omega }
  | Named _ -> omega
  | App { omega_1; omega_2 } ->
      App { omega_1 = invert omega_1; omega_2 = invert omega_2 }
  | Invert omega -> omega

let rec unify (p : pat) (t : term) : (string * term) list option =
  match (p, t) with
  | Named x, _ -> Some [ (x, t) ]
  | Tuple p, Tuple t ->
      let* combined = combine p t in
      let nested = List.map (fun (p, t) -> unify p t) combined in
      let+ nested = bind_all nested in
      List.flatten nested
  | _ -> None

let rec subst ~(from : string) ~(into : term) ~(what : term) : term =
  let subst what = subst ~from ~into ~what in
  match what with
  | Named x when x = from -> into
  | Tuple l -> Tuple (List.map subst l)
  | App { omega; t } -> App { omega; t = subst t }
  | Let { p; t_1; t_2 } when contains from p -> Let { p; t_1 = subst t_1; t_2 }
  | Let { p; t_1; t_2 } -> Let { p; t_1 = subst t_1; t_2 = subst t_2 }
  | LetIso { phi; omega; t } when phi <> from ->
      LetIso { phi; omega; t = subst t }
  | _ -> what

let rec subst_iso ~(from : string) ~(into : iso) ~(what : iso) : iso =
  let subst_iso what = subst_iso ~from ~into ~what in
  match what with
  | Pairs { anot; pairs } when not (contains_pairs from pairs) ->
      let pairs =
        List.map
          (fun (v, e) -> (v, subst_iso_in_expr ~from ~into ~what:e))
          pairs
      in
      Pairs { anot; pairs }
  | Fix { phi; anot; omega } when phi <> from ->
      Fix { phi; anot; omega = subst_iso omega }
  | Lambda { psi; anot; omega } when psi <> from ->
      Lambda { psi; anot; omega = subst_iso omega }
  | Named x when x = from -> into
  | App { omega_1; omega_2 } ->
      App { omega_1 = subst_iso omega_1; omega_2 = subst_iso omega_2 }
  | Invert omega -> Invert (subst_iso omega)
  | _ -> what

and subst_iso_in_expr ~(from : string) ~(into : iso) ~(what : expr) : expr =
  match what with
  | Value _ -> what
  | Let { p_1; omega; p_2; e } ->
      let omega = subst_iso ~from ~into ~what:omega in
      let e =
        if contains from p_1 then e else subst_iso_in_expr ~from ~into ~what:e
      in
      Let { p_1; omega; p_2; e }

let rec subst_iso_in_term ~(from : string) ~(into : iso) ~(what : term) : term =
  let subst_iso_in_term what = subst_iso_in_term ~from ~into ~what in
  let subst_iso what = subst_iso ~from ~into ~what in
  match what with
  | Tuple l -> Tuple (List.map subst_iso_in_term l)
  | App { omega; t } -> App { omega = subst_iso omega; t = subst_iso_in_term t }
  | Let { p; t_1; t_2 } when contains from p ->
      Let { p; t_1 = subst_iso_in_term t_1; t_2 }
  | Let { p; t_1; t_2 } ->
      Let { p; t_1 = subst_iso_in_term t_1; t_2 = subst_iso_in_term t_2 }
  | LetIso { phi; omega; t } when phi = from ->
      LetIso { phi; omega = subst_iso omega; t }
  | LetIso { phi; omega; t } ->
      LetIso { phi; omega = subst_iso omega; t = subst_iso_in_term t }
  | _ -> what

let rec value_of_term : term -> value option = function
  | Unit -> Some Unit
  | Named x -> Some (Named x)
  | Tuple l ->
      let+ l = List.map value_of_term l |> bind_all in
      let lmao : value = Tuple l in
      lmao
  | App { omega = Named c; t : term } ->
      let+ v = value_of_term t in
      Cted { c; v }
  | _ -> None

let match_pair (l : (value * expr) list) (v : value) : (value * expr) option =
  let rec vv : value * value -> bool = function
    | Unit, Unit -> true
    | Named x, _ when is_variable x -> true
    | Named x, Named y -> x = y
    | Cted { c = c_1; v = v_1 }, Cted { c = c_2; v = v_2 } ->
        c_1 = c_2 && vv (v_1, v_2)
    | Tuple l, Tuple r ->
        Option.map (List.for_all vv) (combine l r) |> value_or false
    | _ -> false
  in
  List.find_opt (fun (v', _) -> vv (v', v)) l

let rec unify_value (u : value) (v : value) : (string * value) list =
  match (u, v) with
  | Unit, _ -> []
  | Named x, _ when is_variable x -> [ (x, v) ]
  | Cted { v = v_1; _ }, Cted { v = v_2; _ } -> unify_value v_1 v_2
  | Tuple l, Tuple r ->
      let opt =
        let+ combined = combine l r in
        List.map (fun (u, v) -> unify_value u v) combined |> List.flatten
      in
      value_or [] opt
  | _ -> []

let rec eval (t : term) : term =
  match t with
  | Tuple l -> Tuple (List.map eval l)
  | App { omega; t = u } -> begin
      let omega = eval_iso omega in
      let v = eval u |> value_of_term in
      match (omega, v) with
      | Pairs { pairs; _ }, Some v' ->
          let opt =
            let+ v, e = match_pair pairs v' in
            let unified = unify_value v v' in
            List.fold_left
              (fun t (from, into) ->
                subst ~from ~into:(term_of_value into) ~what:t)
              (term_of_expr e) unified
            |> eval
          in
          value_or t opt
      | _ -> t
    end
  | Let { p; t_1; t_2 } -> begin
      let t_1 = eval t_1 in
      match unify p t_1 with
      | Some unified ->
          List.fold_left
            (fun t (from, into) -> subst ~from ~into ~what:t)
            t_2 unified
          |> eval
      | None -> t
    end
  | LetIso { phi; omega; t } ->
      let omega = eval_iso omega in
      subst_iso_in_term ~from:phi ~into:omega ~what:t |> eval
  | _ -> t

and eval_iso (omega : iso) : iso =
  match omega with
  | Fix { phi; omega = omega'; _ } ->
      subst_iso ~from:phi ~into:omega ~what:omega' |> eval_iso
  | App { omega_1; omega_2 } -> begin
      match eval_iso omega_1 with
      | Lambda { psi; omega; _ } ->
          subst_iso ~from:psi ~into:omega_2 ~what:omega |> eval_iso
      | _ -> omega
    end
  | Invert omega -> invert omega |> eval_iso
  | _ -> omega
