open Util

type any =
  | Unit
  | Product of any list
  | Named of string
  | BiArrow of { a : any; b : any }
  | Arrow of { a : any; b : any }
  | Var of int

type equation = any * any
type subst = { what : int; into : any }
type infered = { a : any; e : equation list }
type elt = Mono of any | Scheme of { forall : int list; a : any }
type context = elt StrMap.t
type generator = { mutable i : int }

let fresh (gen : generator) : int =
  let i = gen.i in
  gen.i <- i + 1;
  i

let rec subst (s : subst) : any -> any = function
  | Var x when x = s.what -> s.into
  | Product l -> Product (List.map (subst s) l)
  | BiArrow { a; b } -> BiArrow { a = subst s a; b = subst s b }
  | Arrow { a; b } -> Arrow { a = subst s a; b = subst s b }
  | otherwise -> otherwise

let subst_in_context (s : subst) : context -> context =
  StrMap.map
    begin
      function
      | Mono a -> Mono (subst s a)
      | Scheme { forall; a } when List.for_all (( <> ) s.what) forall ->
          Scheme { forall; a = subst s a }
      | otherwise -> otherwise
    end

let subst_in_equations (s : subst) : equation list -> equation list =
  List.map (fun (a, b) -> (subst s a, subst s b))

let instantiate (gen : generator) : elt -> any = function
  | Mono a -> a
  | Scheme { forall; a } ->
      List.fold_left
        (fun a what -> subst { what; into = Var (fresh gen) } a)
        a forall

let rec occurs (x : int) : any -> bool = function
  | Product l -> List.exists (occurs x) l
  | BiArrow { a; b } | Arrow { a; b } -> occurs x a || occurs x b
  | Var y -> x = y
  | _ -> false

let rec unify : equation list -> subst list myresult = function
  | [] -> Ok []
  | e :: e' -> begin
      match e with
      | a, b when a = b -> unify e'
      | Var x, b when occurs x b |> not ->
          let s = { what = x; into = b } in
          let++ unified = subst_in_equations s e' |> unify in
          s :: unified
      | a, Var x when occurs x a |> not ->
          let s = { what = x; into = a } in
          let++ unified = subst_in_equations s e' |> unify in
          s :: unified
      | Product l, Product r when List.compare_lengths l r = 0 ->
          List.combine l r @ e' |> unify
      | BiArrow { a = a_1; b = b_1 }, BiArrow { a = a_2; b = b_2 } ->
          (a_1, a_2) :: (b_1, b_2) :: e' |> unify
      | Arrow { a = a_1; b = b_1 }, Arrow { a = a_2; b = b_2 } ->
          (a_1, a_2) :: (b_1, b_2) :: e' |> unify
      | _ -> Error "unable to unify"
    end

let rec context_of_pat (gen : generator) (p : Types.pat) : any * any StrMap.t =
  match p with
  | Named x ->
      let fresh = Var (fresh gen) in
      (fresh, StrMap.singleton x fresh)
  | Tuple l ->
      let base_types, binds = List.map (context_of_pat gen) l |> List.split in
      (Product base_types, union_list binds)

let find_generalizable : any -> context -> int list =
  let module IntSet = Set.Make (Int) in
  let rec find_in_any =
    function
      | Unit -> []
      | Product l -> List.map find_in_any l |> List.flatten
      | Named _ -> []
      | BiArrow { a; b } | Arrow { a; b } ->
          (find_in_any a) @ (find_in_any b)
      | Var x -> [x]
  in
  let rec find_in_context =
    function
      | Mono a -> find_in_any a
      | Scheme { forall; a } -> find_in_any 

let generalize (e : equation list) (ctx : context) (p : Types.pat) (a : any)
    (gen : generator) : (context * equation) myresult =
  let++ substs = unify e in
  let u = List.fold_left (fun a s -> subst s a) a substs in
  let ctx' = List.fold_left (fun ctx s -> subst_in_context s ctx) ctx substs in
  let product, binds = context_of_pat gen p in

let rec infer_term (t : Types.term) (gen : generator) (ctx : context) :
    infered myresult =
  match t with
  | Unit -> Ok { a = Unit; e = [] }
  | Named x ->
      let++ elt = find_res x ctx in
      { a = instantiate gen elt; e = [] }
  | Tuple l ->
      let++ infered = List.map (fun t -> infer_term t gen ctx) l |> bind_all in
      let product = List.map (fun { a; _ } -> a) infered in
      let equations = List.map (fun { e; _ } -> e) infered |> List.flatten in
      let fresh = Var (fresh gen) in
      { a = fresh; e = (fresh, Product product) :: equations }
  | App { omega; t } ->
      let** { a = a_1; e = e_1 } = infer_iso omega gen ctx in
      let++ { a = a_2; e = e_2 } = infer_term t gen ctx in
      let e = e_1 @ e_2 in
      let fresh = Var (fresh gen) in
      { a = fresh; e = (a_1, BiArrow { a = a_2; b = fresh }) :: e }
  | Let { p; t_1; t_2 } -> asdf

(*
| Unit
| Named of string
| Tuple of term list
| App of { omega : iso; t : term }
| Let of { p : pat; t_1 : term; t_2 : term }
| LetIso of { phi : string; omega : iso; t : term }
*)

(*
| Pairs of { annot : iso_type; pairs : (value * expr) list }
| Fix of { phi : string; annot : iso_type; omega : iso }
| Lambda of { psi : string; annot : iso_type; omega : iso }
| Named of string
| App of { omega_1 : iso; omega_2 : iso }
| Invert of iso
*)
and infer_iso (omega : Types.iso) (gen : generator) (ctx : context) :
    infered myresult =
  a

let rec invert_iso_type : Types.iso_type -> Types.iso_type = function
  | BiArrow { a; b } -> BiArrow { a = b; b = a }
  | Arrow { t_1; t_2 } ->
      let t_1 = invert_iso_type t_1 in
      let t_2 = invert_iso_type t_2 in
      Arrow { t_1; t_2 }
(*
let rec is_orthogonal (u : value) (v : value) : string option =
  let msg =
    lazy (Some (show_value u ^ " and " ^ show_value v ^ " are not orthogonal"))
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


let rec unify_value (ctx : context) (v : value) (a : base_type) : delta myresult
    =
  let msg = lazy (show_value v ^ " with type " ^ show_base_type a) in
  match (v, a) with
  | Unit, Unit -> Ok StrMap.empty
  | Named x, _ when is_variable x -> Ok (StrMap.singleton x a)
  | Named x, _ ->
      let** b =
        StrMap.find_opt x ctx.delta
        |> Option.to_result
             ~none:(x ^ " was not found in the context of base_type")
      in
      if a = b then Ok StrMap.empty
      else
        Error
          (x ^ " is expected to have type " ^ show_base_type a
         ^ " but it has type " ^ show_base_type b)
  | Cted { c; v }, b' -> begin
      let** omega =
        StrMap.find_opt c ctx.psi
        |> Option.to_result
             ~none:(c ^ " was not found in the context of iso_type")
      in
      match omega with
      | BiArrow { a; b } ->
          if b = b' then unify_value ctx v a
          else
            Error
              (c ^ " is expected to have type _ <-> " ^ show_base_type b'
             ^ " but it has type " ^ show_base_type a ^ " <-> "
             ^ show_base_type b)
      | Arrow _ ->
          Error
            (c ^ " is expected to have type _ <-> " ^ show_base_type b'
           ^ " but it has type " ^ show_iso_type omega)
    end
  | Tuple t, Product p ->
      let** combined =
        combine t p
        |> Option.to_result ~none:("arity mismatch: " ^ Lazy.force msg)
      in
      let** list =
        List.map (fun (v, a) -> unify_value ctx v a) combined |> bind_all
      in
      List.fold_left
        (fun acc delta -> Result.bind acc (fun acc -> union_nodup acc delta))
        (Ok StrMap.empty) list
  | _ -> Error ("unable to unify " ^ Lazy.force msg)

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

let rec unify_pat (p : pat) (a : base_type) : base_type StrMap.t myresult =
  let msg = lazy (show_pat p ^ " and type " ^ show_base_type a) in
  match (p, a) with
  | Named x, _ -> Ok (StrMap.singleton x a)
  | Tuple tpl, Product prd ->
      let** combined =
        combine tpl prd
        |> Option.to_result ~none:("arity mismatch: " ^ Lazy.force msg)
      in
      let** list =
        List.map (fun (p, a) -> unify_pat p a) combined |> bind_all
      in
      List.fold_left
        (fun acc delta -> Result.bind acc (fun acc -> union_nodup acc delta))
        (Ok StrMap.empty) list
  | _ -> Error ("unable to unify " ^ Lazy.force msg)

let rec infer_base_in_expr (ctx : context) (e : expr) : base_type myresult =
  match e with
  | Value v -> infer_base ctx (term_of_value v)
  | Let { p_1; omega; e; _ } ->
      let** omega' = infer_iso ctx omega in
      let** b =
        match omega' with
        | BiArrow { b; _ } -> Ok b
        | Arrow _ ->
            Error
              (show_iso omega
             ^ " is expected to have type _ <-> _ but it has type "
             ^ show_iso_type omega')
      in
      let** unified = unify_pat p_1 b in
      let extended = union ~weak:ctx.delta ~strong:unified in
      infer_base_in_expr { psi = ctx.psi; delta = extended } e

and infer_base (ctx : context) (t : term) : base_type myresult =
  match t with
  | Unit -> Ok Unit
  | Named x ->
      StrMap.find_opt x ctx.delta
      |> Option.to_result
           ~none:(x ^ " was not found in the context of base_type")
  | Tuple l ->
      let++ l = List.map (infer_base ctx) l |> bind_all in
      Product l
  | App { omega; t } -> begin
      let** omega' = infer_iso ctx omega in
      let** t = infer_base ctx t in
      match omega' with
      | BiArrow { a; b } ->
          if a = t then Ok b
          else
            Error
              (show_iso omega ^ " is expected to have type " ^ show_base_type t
             ^ " <-> _ but it has type " ^ show_base_type a ^ " <-> "
             ^ show_base_type b)
      | Arrow _ ->
          Error
            (show_iso omega ^ " is expected to have type " ^ show_base_type t
           ^ " <-> _ but it has type " ^ show_iso_type omega')
    end
  | Let { p; t_1; t_2 } ->
      let** a = infer_base ctx t_1 in
      let** unified = unify_pat p a in
      let extended = union ~weak:ctx.delta ~strong:unified in
      infer_base { psi = ctx.psi; delta = extended } t_2
  | LetIso { phi; omega; t } ->
      let** omega = infer_iso ctx omega in
      let extended = StrMap.add phi omega ctx.psi in
      infer_base { psi = extended; delta = ctx.delta } t

and infer_iso (ctx : context) (omega : iso) : iso_type myresult =
  match omega with
  | Pairs { annot = BiArrow { a; b }; pairs } ->
      let infer_pairs a b pairs =
        let well_typed (v, e) =
          let infered =
            let** unified = unify_value ctx v a in
            let extended = union ~weak:ctx.delta ~strong:unified in
            infer_base_in_expr { psi = ctx.psi; delta = extended } e
          in
          match infered with
          | Ok b' ->
              if b' = b then None
              else
                Some
                  (show_expr e ^ " is expected to have type " ^ show_base_type b
                 ^ " but it has type " ^ show_base_type b')
          | Error e -> Some e
        in
        match List.find_map well_typed pairs with
        | Some x -> Error x
        | None -> (
            let is_orthogonal_v =
              List.map (fun (v, _) -> v) pairs |> for_all_pairs is_orthogonal
            in
            let is_orthogonal_e =
              List.map (fun (_, e) -> value_of_expr e) pairs
              |> for_all_pairs is_orthogonal
            in
            match (is_orthogonal_e, is_orthogonal_v) with
            | Some e, _ | _, Some e -> Error e
            | None, None -> Ok (BiArrow { a; b }))
      in
      let inverted = invert_pairs pairs in
      let** _ =
        infer_pairs b a inverted
        |> Result.map_error (fun e ->
               e ^ "\nin inverted pairs: " ^ show_pairs inverted)
      in
      infer_pairs a b pairs
  | Pairs _ -> Error "unreachable (Pairs have non-biarrow type)"
  | Fix { phi; annot; omega = omega' } ->
      let extended = extend ctx.psi [ (phi, annot) ] in
      let** omega = infer_iso { psi = extended; delta = ctx.delta } omega' in
      if omega = annot then Ok annot
      else
        Error
          (show_iso omega' ^ " is expected to have type " ^ show_iso_type annot
         ^ " but it has type " ^ show_iso_type omega)
  | Lambda { psi; annot; omega } ->
      let extended = extend ctx.psi [ (psi, annot) ] in
      let++ t_2 = infer_iso { psi = extended; delta = ctx.delta } omega in
      Arrow { t_1 = annot; t_2 }
  | Named x ->
      StrMap.find_opt x ctx.psi
      |> Option.to_result ~none:(x ^ " was not found in the context of iso_type")
  | App { omega_1; omega_2 } -> begin
      let** omega_1' = infer_iso ctx omega_1 in
      let** omega_2' = infer_iso ctx omega_2 in
      match omega_1' with
      | Arrow { t_1; t_2 } ->
          if t_1 = omega_2' then Ok t_2
          else
            Error
              (show_iso omega_1 ^ " is expected to have type "
             ^ show_iso_type omega_2' ^ " -> _ but it has type "
             ^ show_iso_type t_1 ^ " -> " ^ show_iso_type t_2)
      | BiArrow _ ->
          Error
            (show_iso omega_1
           ^ " is expected to have type _ -> _ but it has type "
           ^ show_iso_type omega_1')
    end
  | Invert omega -> Result.map invert_iso_type (infer_iso ctx omega)

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
*)
