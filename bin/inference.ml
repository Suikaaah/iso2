open Util

type any =
  | Unit
  | Product of any list
  | Named of string
  | BiArrow of { a : any; b : any }
  | Arrow of { a : any; b : any }
  | Var of int
  | Ctor of any list * string
  | Inverted of any

type equation = any * any
type subst = { what : int; into : any }
type inferred_pair = { a_v : any; a_e : any; e : equation list }
type inferred = { a : any; e : equation list }
type elt = Mono of any | Scheme of { forall : int list; a : any }
type context = elt StrMap.t
type generator = { mutable i : int }

let rec invert_iso_type : any -> any myresult = function
  | BiArrow { a; b } -> Ok (BiArrow { a = b; b = a })
  | Arrow { a; b } ->
      let** a = invert_iso_type a in
      let++ b = invert_iso_type b in
      Arrow { a; b }
  (* is this needed? *)
  | Inverted a -> Ok a
  | otherwise -> Error (show_any otherwise ^ " is not an iso type")

and base_of_any : any -> Types.base_type myresult = function
  | Unit -> Ok Types.Unit
  | Product l ->
      let++ l = List.map base_of_any l |> bind_all in
      Types.Product l
  | Named x -> Ok (Types.Named x)
  | Var x -> Ok (Types.Var ("'" ^ string_of_int x))
  | Ctor (l, x) ->
      let++ l = List.map base_of_any l |> bind_all in
      Types.Ctor (l, x)
  | _ -> Error "not a base type"

and iso_of_any : any -> Types.iso_type myresult = function
  | BiArrow { a; b } ->
      let** a = base_of_any a in
      let++ b = base_of_any b in
      Types.BiArrow { a; b }
  | Arrow { a; b } ->
      let** t_1 = iso_of_any a in
      let++ t_2 = iso_of_any b in
      Types.Arrow { t_1; t_2 }
  | Var x -> Ok (Types.Var x)
  | Inverted a ->
      let** inv = invert_iso_type a in
      iso_of_any inv
  | _ -> Error "not an iso type"

(* todo: update *)
and show_any : any -> string = function
  | Inverted a -> "invert (" ^ show_any a ^ ")"
  | a -> begin
      match base_of_any a with
      | Ok a -> Types.show_base_type a
      | Error _ -> begin
          match iso_of_any a with
          | Ok a -> Types.show_iso_type a
          | Error _ -> "unreachable (neither base or iso)"
        end
    end

let show_elt : elt -> string = function
  | Mono a -> show_any a
  | Scheme { forall; a } ->
      "forall "
      ^ show_list (fun x -> "'" ^ string_of_int x) forall
      ^ ". " ^ show_any a

let show_context (ctx : context) : string =
  StrMap.to_list ctx |> show_list (fun (k, e) -> k ^ " : " ^ show_elt e)

let show_equation ((a, b) : equation) : string = show_any a ^ " = " ^ show_any b
let show_equations : equation list -> string = show_list show_equation

let fresh (gen : generator) : int =
  let i = gen.i in
  gen.i <- i + 1;
  i

let rec subst (s : subst) : any -> any = function
  | Var x when x = s.what -> s.into
  | Product l -> Product (List.map (subst s) l)
  | BiArrow { a; b } -> BiArrow { a = subst s a; b = subst s b }
  | Arrow { a; b } -> Arrow { a = subst s a; b = subst s b }
  | Ctor (l, x) -> Ctor (List.map (subst s) l, x)
  | Inverted a -> Inverted (subst s a)
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
  | Product l | Ctor (l, _) -> List.exists (occurs x) l
  | BiArrow { a; b } | Arrow { a; b } -> occurs x a || occurs x b
  | Var y -> x = y
  | Inverted a -> occurs x a
  | _ -> false

let rec unify : equation list -> subst list myresult = function
  | [] -> Ok []
  | e :: e' -> begin
      match e with
      | a, b when a = b -> unify e'
      | Inverted a, Inverted b -> (a, b) :: e' |> unify
      | Inverted i, BiArrow { a; b } ->
          (i, BiArrow { a = b; b = a }) :: e' |> unify
      | BiArrow { a; b }, Inverted i ->
          (i, BiArrow { a = b; b = a }) :: e' |> unify
      | Inverted i, Arrow { a; b } ->
          let** a = invert_iso_type a in
          let** b = invert_iso_type b in
          (i, Arrow { a; b }) :: e' |> unify
      | Arrow { a; b }, Inverted i ->
          let** a = invert_iso_type a in
          let** b = invert_iso_type b in
          (i, Arrow { a; b }) :: e' |> unify
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
      | Ctor (l_1, x_1), Ctor (l_2, x_2)
        when x_1 = x_2 && List.compare_lengths l_1 l_2 = 0 ->
          List.combine l_1 l_2 @ e' |> unify
      | a, b -> Error ("unable to unify " ^ show_any a ^ " and " ^ show_any b)
    end

let finalize ({ a; e } : inferred) : any myresult =
  let++ substs = unify e in
  List.fold_left (fun a s -> subst s a) a substs

let rec context_of_pat (gen : generator) (p : Types.pat) : any * any StrMap.t =
  match p with
  | Named x ->
      let fresh = Var (fresh gen) in
      (fresh, StrMap.singleton x fresh)
  | Tuple l ->
      let base_types, binds = List.map (context_of_pat gen) l |> List.split in
      (Product base_types, union_list binds)

(* todo: optimization *)
let find_generalizable (a : any) (ctx : context) : int list =
  let module IntSet = Set.Make (Int) in
  let rec find_in_any = function
    | Unit | Named _ -> IntSet.empty
    | Product l | Ctor (l, _) ->
        List.fold_left
          (fun acc a -> find_in_any a |> IntSet.union acc)
          IntSet.empty l
    | BiArrow { a; b } | Arrow { a; b } ->
        IntSet.union (find_in_any a) (find_in_any b)
    | Var x -> IntSet.singleton x
    | Inverted a -> find_in_any a
  in
  let find_in_context ctx =
    let f = function
      | Mono a -> find_in_any a
      | Scheme { forall; a } ->
          IntSet.diff (find_in_any a) (IntSet.of_list forall)
    in
    StrMap.fold (fun _ a acc -> f a |> IntSet.union acc) ctx IntSet.empty
  in
  IntSet.diff (find_in_any a) (find_in_context ctx) |> IntSet.to_list

let generalize (e : equation list) (ctx : context) (p : Types.pat) (a : any)
    (gen : generator) : (context * equation) myresult =
  let++ substs = unify e in
  let u = List.fold_left (fun a s -> subst s a) a substs in
  let ctx = List.fold_left (fun ctx s -> subst_in_context s ctx) ctx substs in
  let product, binds = context_of_pat gen p in
  let generalized =
    let forall = find_generalizable u ctx in
    StrMap.map (fun a -> Scheme { forall; a }) binds
  in
  (union ~weak:ctx ~strong:generalized, (u, product))

let generalize_iso (e : equation list) (ctx : context) (phi : string) (a : any)
    : context myresult =
  let++ substs = unify e in
  let u = List.fold_left (fun a s -> subst s a) a substs in
  let ctx = List.fold_left (fun ctx s -> subst_in_context s ctx) ctx substs in
  let generalized = Scheme { forall = find_generalizable u ctx; a = u } in
  StrMap.add phi generalized ctx

let rec extract_named (gen : generator) (v : Types.value) : context =
  match v with
  | Named x when is_variable x ->
      let var = Var (fresh gen) in
      StrMap.singleton x (Mono var)
  | Unit | Named _ -> StrMap.empty
  | Cted { v; _ } -> extract_named gen v
  | Tuple l -> union_list (List.map (extract_named gen) l)

let rec is_orthogonal (u : Types.value) (v : Types.value) : unit myresult =
  let rec setup : Types.value -> _ = function
    | Unit -> StrMap.empty
    | Named x when is_variable x -> StrMap.singleton x None
    | Named _ -> StrMap.empty
    | Cted { v; _ } -> setup v
    | Tuple l ->
        List.map setup l
        |> List.fold_left
             (StrMap.union (fun _ _ _ -> Some (Some None)))
             StrMap.empty
  in

  let map_u = setup u |> ref in
  let map_v = setup v |> ref in

  let mult_occ x map =
    StrMap.find_opt x map |> Option.map Option.is_some
    (* unreachable *) |> Option.value ~default:false
  in

  let fatal x = Error (`Fatal x) in
  let idk x = Error (`Idk x) in
  let is_fatal = function Error (`Fatal _) -> true | _ -> false in
  let is_idk = function Error (`Idk _) -> true | _ -> false in

  let is_okay x v map =
    match StrMap.find_opt x !map with
    | None -> Ok ()
    (* one occurrence *)
    | Some None -> Ok ()
    (* more than one but not memoed *)
    | Some (Some None) ->
        map := StrMap.add x (Some (Some v)) !map;
        Ok ()
    (* memoed *)
    | Some (Some (Some v')) ->
        is_orthogonal v v' |> Result.map_error (fun x -> `Fatal x)
  in

  let msg =
    lazy
      (Types.show_value u ^ " and " ^ Types.show_value v ^ " are not orthogonal"
      |> idk)
  in

  let rec body (u : Types.value) (v : Types.value) =
    match (u, v) with
    | Unit, Unit -> Lazy.force msg
    | Named x, _ when is_variable x ->
        if mult_occ x !map_u then is_okay x v map_u else Lazy.force msg
    | _, Named x when is_variable x ->
        if mult_occ x !map_v then is_okay x u map_v else Lazy.force msg
    | Named x, Named y when x = y -> Lazy.force msg
    | Cted { c = c_1; v = v_1 }, Cted { c = c_2; v = v_2 } ->
        if c_1 = c_2 then body v_1 v_2 else Ok ()
    | Tuple l, Tuple r ->
        let combined = combine l r in
        begin
          match combined with
          | Some combined ->
              let mapped = List.map (fun (u, v) -> body u v) combined in
              let all_idk = List.for_all is_idk mapped in
              let exists_fatal = List.exists is_fatal mapped in
              let is_error = all_idk || exists_fatal in
              if is_error then Lazy.force msg else Ok ()
          | None ->
              "arity mismatch: " ^ Types.show_value u ^ " and "
              ^ Types.show_value v
              |> fatal
        end
    | _ -> Ok ()
  in
  body u v |> Result.map_error (function `Fatal e | `Idk e -> e)

let invert_pairs (pairs : (Types.value * Types.expr) list) :
    (Types.value * Types.expr) list =
  let rec invert_expr (e : Types.expr) (acc : Types.expr) =
    match e with
    | Value v -> (v, acc)
    | Let { p_1; omega; p_2; e } ->
        invert_expr e
          (Let { p_1 = p_2; omega = Invert omega; p_2 = p_1; e = acc })
  in
  let invert_pair (v, e) = invert_expr e (Value v) in
  List.map invert_pair pairs

let check_pair ((v, e) : Types.value * Types.expr) : unit myresult =
  let set = ref StrSet.empty in
  let add x = set := StrSet.add x !set in
  let add_unique x =
    match StrSet.find_opt x !set with
    | Some _ -> Error (x ^ " is used more than once")
    | None -> Ok (add x)
  in
  let ensure_existence x =
    match StrSet.find_opt x !set with
    | Some _ -> Ok ()
    | None -> Error (x ^ " is not in context")
  in
  let rec collect_in_value = function
    | Types.Cted { v; _ } -> collect_in_value v
    | Types.Unit -> ()
    | Types.Named x when is_variable x -> add x
    | Types.Named _ -> ()
    | Types.Tuple l -> List.iter collect_in_value l
  in
  let rec check_in_value = function
    | Types.Cted { v; _ } -> check_in_value v
    | Types.Unit -> Ok ()
    | Types.Named x when is_variable x -> ensure_existence x
    | Types.Named _ -> Ok ()
    | Types.Tuple l ->
        let++ _ = List.map check_in_value l |> bind_all in
        ()
  in
  let rec collect_in_pat : Types.pat -> _ = function
    | Types.Named x -> add_unique x
    | Types.Tuple l ->
        let++ _ = List.map collect_in_pat l |> bind_all in
        ()
  in
  let rec check_in_pat : Types.pat -> _ = function
    | Types.Named x -> ensure_existence x
    | Types.Tuple l ->
        let++ _ = List.map check_in_pat l |> bind_all in
        ()
  in
  let rec check_in_expr : Types.expr -> _ = function
    | Types.Let { p_1; p_2; e; _ } ->
        let** _ = check_in_pat p_2 in
        let** _ = collect_in_pat p_1 in
        check_in_expr e
    | Types.Value v -> check_in_value v
  in
  collect_in_value v;
  check_in_expr e

let rec infer_pair (gen : generator) (ctx : context)
    ((v, e) : Types.value * Types.expr) : inferred_pair myresult =
  let ctx = union ~weak:ctx ~strong:(extract_named gen v) in
  let** { a = a_v; e = e_v } = infer_term (Types.term_of_value v) gen ctx in
  let++ { a = a_e; e = e_e } = infer_expr e gen ctx in
  { a_v; a_e; e = e_v @ e_e }

and infer_term (t : Types.term) (gen : generator) (ctx : context) :
    inferred myresult =
  match t with
  | Unit -> Ok { a = Unit; e = [] }
  | Named x ->
      let++ elt = find_res x ctx in
      { a = instantiate gen elt; e = [] }
  | Tuple l ->
      let++ inferred = List.map (fun t -> infer_term t gen ctx) l |> bind_all in
      let product = List.map (fun { a; _ } -> a) inferred in
      let e = List.map (fun { e; _ } -> e) inferred |> List.flatten in
      { a = Product product; e }
  | App { omega; t } ->
      let** { a = a_1; e = e_1 } = infer_iso omega gen ctx in
      let++ { a = a_2; e = e_2 } = infer_term t gen ctx in
      let e = e_1 @ e_2 in
      let fresh = Var (fresh gen) in
      { a = fresh; e = (a_1, BiArrow { a = a_2; b = fresh }) :: e }
  | Let { p; t_1; t_2 } ->
      let** { a = a_1; e = e_1 } = infer_term t_1 gen ctx in
      let** ctx, e = generalize e_1 ctx p a_1 gen in
      let++ { a = a_2; e = e_2 } = infer_term t_2 gen ctx in
      { a = a_2; e = (e :: e_1) @ e_2 }
  | LetIso { phi; omega; t } ->
      let** { a = a_1; e = e_1 } = infer_iso omega gen ctx in
      let** ctx = generalize_iso e_1 ctx phi a_1 in
      let++ { a = a_2; e = e_2 } = infer_term t gen ctx in
      { a = a_2; e = e_1 @ e_2 }

and infer_expr (expr : Types.expr) (gen : generator) (ctx : context) :
    inferred myresult =
  match expr with
  | Value v -> infer_term (Types.term_of_value v) gen ctx
  | Let { p_1; omega; p_2; e = expr } ->
      let t_1 = Types.App { omega; t = Types.term_of_pat p_2 } in
      let** { a = a_1; e = e_1 } = infer_term t_1 gen ctx in
      let** ctx, e = generalize e_1 ctx p_1 a_1 gen in
      let++ { a = a_2; e = e_2 } = infer_expr expr gen ctx in
      { a = a_2; e = (e :: e_1) @ e_2 }

and infer_iso (omega : Types.iso) (gen : generator) (ctx : context) :
    inferred myresult =
  match omega with
  | Pairs p ->
      let check_orth p =
        let** _ =
          List.map
            (fun (v, e) ->
              check_pair (v, e)
              |> Result.map_error (fun x ->
                     x ^ " in pair " ^ Types.show_value v ^ " <-> "
                     ^ Types.show_expr e))
            p
          |> bind_all
        in
        let left = List.map (fun (v, _) -> v) p in
        let right = List.map (fun (_, e) -> Types.value_of_expr e) p in
        let** () = for_all_pairs is_orthogonal left in
        for_all_pairs is_orthogonal right
      in
      let infer p =
        let++ pairs = List.map (infer_pair gen ctx) p |> bind_all in
        let types_v, types_e =
          List.map (fun { a_v; a_e; _ } -> (a_v, a_e)) pairs |> List.split
        in
        let es =
          List.map (fun ({ e; _ } : inferred_pair) -> e) pairs |> List.flatten
        in
        let a = List.hd types_v in
        let b = List.hd types_e in
        let types_v' = List.drop 1 types_v @ [ a ] in
        let types_e' = List.drop 1 types_e @ [ b ] in
        let e_v = List.map2 (fun a b -> (a, b)) types_v types_v' in
        let e_e = List.map2 (fun a b -> (a, b)) types_e types_e' in
        { a = BiArrow { a; b }; e = e_v @ e_e @ es }
      in
      let** () = check_orth p in
      let** () = invert_pairs p |> check_orth in
      infer p
  | Fix { phi; omega } ->
      let fresh = Var (fresh gen) in
      let ctx = StrMap.add phi (Mono fresh) ctx in
      let++ { a; e } = infer_iso omega gen ctx in
      { a; e = (fresh, a) :: e }
  | Lambda { psi; omega } ->
      let fresh = Var (fresh gen) in
      let ctx = StrMap.add psi (Mono fresh) ctx in
      let++ { a; e } = infer_iso omega gen ctx in
      { a = Arrow { a = fresh; b = a }; e }
  | Named omega ->
      let++ elt = find_res omega ctx in
      { a = instantiate gen elt; e = [] }
  | App { omega_1; omega_2 } ->
      let** { a = a_1; e = e_1 } = infer_iso omega_1 gen ctx in
      let++ { a = a_2; e = e_2 } = infer_iso omega_2 gen ctx in
      let e = e_1 @ e_2 in
      let fresh = Var (fresh gen) in
      { a = fresh; e = (a_1, Arrow { a = a_2; b = fresh }) :: e }
  | Invert omega ->
      let++ { a; e } = infer_iso omega gen ctx in
      let fresh = Var (fresh gen) in
      { a = fresh; e = (fresh, Inverted a) :: e }

let rec any_of_base (map : int StrMap.t) : Types.base_type -> any myresult =
  function
  | Unit -> Ok Unit
  | Product l ->
      let++ l = List.map (any_of_base map) l |> bind_all in
      Product l
  | Named x -> Ok (Named x)
  | Var x ->
      let++ x = find_res x map in
      Var x
  | Ctor (l, x) ->
      let++ l = List.map (any_of_base map) l |> bind_all in
      Ctor (l, x)

let build_ctx (gen : generator) (defs : Types.typedef list) : context myresult =
  let build Types.{ vars; t; vs } =
    let map =
      List.fold_left
        (fun acc x -> StrMap.add x (fresh gen) acc)
        StrMap.empty vars
    in
    let** forall = List.map (fun x -> find_res x map) vars |> bind_all in
    let inner =
      match forall with
      | [] -> Named t
      | _ -> Ctor (List.map (fun x -> Var x) forall, t)
    in
    let f = function
      | Types.Value x -> Ok (x, Scheme { forall; a = inner })
      | Types.Iso { c; a } ->
          let++ a = any_of_base map a in
          let inner = BiArrow { a; b = inner } in
          (c, Scheme { forall; a = inner })
    in
    List.map f vs |> bind_all
  in
  let++ acc = List.map build defs |> bind_all in
  acc |> List.flatten |> StrMap.of_list
