open Types
open Util

type psi = iso_type StrMap.t
type delta = base_type StrMap.t
type context = { psi : psi; delta : delta }

let rec invert_iso_type : iso_type -> iso_type = function
  | BiArrow { a; b } -> BiArrow { a = b; b = a }
  | Arrow { t_1; t_2 } ->
      let t_1 = invert_iso_type t_1 in
      let t_2 = invert_iso_type t_2 in
      Arrow { t_1; t_2 }

let rec unify_pat (p : pat) (a : base_type) : (string * base_type) list option =
  match (p, a) with
  | Named x, _ -> Some [ (x, a) ]
  | Tuple t, Product p ->
      let* combined = combine t p in
      let nested = List.map (fun (p, a) -> unify_pat p a) combined in
      let+ nested = bind_all nested in
      List.flatten nested
  | _ -> None

let rec unify_value (ctx : context) (v : value) (a : base_type) :
    (string * base_type) list option =
  match (v, a) with
  | Unit, Unit -> Some []
  | Named x, _ -> Some [ (x, a) ]
  | Cted { c; v }, Sum _ -> begin
      let* c = StrMap.find_opt c ctx.psi in
      match c with BiArrow { a; _ } -> unify_value ctx v a | Arrow _ -> None
    end
  | Tuple t, Product p ->
      let* combined = combine t p in
      let nested = List.map (fun (v, a) -> unify_value ctx v a) combined in
      let+ nested = bind_all nested in
      List.flatten nested
  | _ -> None

let rec infer_base_in_expr (ctx : context) (e : expr) : base_type option =
  match e with
  | Value v -> infer_base ctx (term_of_value v)
  | Let { p_1; omega; e; _ } ->
      let* omega = infer_iso ctx omega in
      let* b = match omega with BiArrow { b; _ } -> Some b | _ -> None in
      let* unified = unify_pat p_1 b in
      let extended = extend ctx.delta unified in
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
      let extended = extend ctx.delta unified in
      infer_base { psi = ctx.psi; delta = extended } t_2
  | LetIso { phi; omega; t } ->
      let* omega = infer_iso ctx omega in
      let extended = StrMap.add phi omega ctx.psi in
      infer_base { psi = extended; delta = ctx.delta } t

and infer_iso (ctx : context) (omega : iso) : iso_type option =
  match omega with
  | Pairs { anot; _ } | Fix { anot; _ } -> Some anot
  | Lambda { psi; anot; omega } ->
      let extended = extend ctx.psi [ (psi, anot) ] in
      infer_iso { psi = extended; delta = ctx.delta } omega
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
