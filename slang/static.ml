exception
  Type_error of { loc : Past.Loc.t; expecting : Type.hole; found : Type.t }

let expected loc expecting found = raise (Type_error { loc; expecting; found })

type env = (Ast.var * Type.t) list

let rec elab (env : env) (e : Past.t) : Ast.t * Type.t =
  let elab_as env (e : Past.t) (expecting : Type.t) =
    let elab, found = elab env e in
    if found = expecting then elab
    else expected e.loc (expecting :> Type.hole) found
  in
  match e.expr with
  | Unit -> (Unit, `Unit)
  | What -> (UnaryOp (`Read, Unit), `Int)
  | Integer i -> (Integer i, `Int)
  | Boolean b -> (Boolean b, `Bool)
  | Var x -> (
      match List.assoc_opt x env with
      | Some t -> (Var x, t)
      | None -> raise Not_found)
  | Seq el ->
      let el' = List.map (elab env) el in
      (Seq (List.map fst el'), List.rev el' |> List.hd |> snd)
  | While (e1, e2) ->
      let e1' = elab_as env e1 `Bool in
      let e2' = elab_as env e2 `Unit in
      (While (e1', e2'), `Unit)
  | Ref e ->
      let e', t = elab env e in
      (Ref e', `Ref t)
  | Deref e -> (
      match elab env e with
      | e', `Ref t -> (Deref e', t)
      | _, t -> expected e.loc (`Ref (`Variable "a")) t)
  | Assign (e1, e2) ->
      let e1', t =
        match elab env e1 with
        | e1', `Ref t -> (e1', t)
        | _, t -> expected e1.loc (`Ref (`Variable "a")) t
      in
      let e2' = elab_as env e2 t in
      (Assign (e1', e2'), `Unit)
  | UnaryOp (op, e) -> (
      let e', t = elab env e in
      let exactly (a : Type.t) (b : Type.t) =
        if t <> a then expected e.loc (a :> Type.hole) t else b
      in
      match op with
      | `Neg -> (UnaryOp (`Neg, e'), exactly `Int `Int)
      | `Not -> (UnaryOp (`Not, e'), exactly `Bool `Bool))
  | BinaryOp (e1, op, e2) -> (
      let e1', t1 = elab env e1 in
      let e2', t2 = elab env e2 in
      let exactly a b c =
        if t1 <> a then expected e1.loc (a :> Type.hole) t1
        else if t2 <> b then expected e2.loc (b :> Type.hole) t2
        else c
      in
      match op with
      | `Add -> (BinaryOp (e1', `Add, e2'), exactly `Int `Int `Int)
      | `Sub -> (BinaryOp (e1', `Sub, e2'), exactly `Int `Int `Int)
      | `Mul -> (BinaryOp (e1', `Mul, e2'), exactly `Int `Int `Int)
      | `Div -> (BinaryOp (e1', `Div, e2'), exactly `Int `Int `Int)
      | `Lt -> (BinaryOp (e1', `Lt, e2'), exactly `Int `Int `Bool)
      | `And -> (BinaryOp (e1', `And, e2'), exactly `Bool `Bool `Bool)
      | `Or -> (BinaryOp (e1', `Or, e2'), exactly `Bool `Bool `Bool)
      | `Eq ->
          (* Monomorphise the equality operator *)
          let op' : Ast.binary_op =
            match t1 with
            | `Int -> if t2 <> `Int then expected e2.loc `Int t2 else `Eqi
            | `Bool -> if t2 <> `Bool then expected e2.loc `Bool t2 else `Eqb
            | _ -> expected e1.loc (`Variable "int_or_bool") t1
          in
          (BinaryOp (e1', op', e2'), `Bool))
  | If (pe1, pe2, pe3) ->
      let ie1 = elab_as env pe1 `Bool in
      let ie2, t2 = elab env pe2 in
      let ie3 = elab_as env pe3 t2 in
      (If (ie1, ie2, ie3), t2)
  | Pair (e1, e2) ->
      let e1', t1 = elab env e1 in
      let e2', t2 = elab env e2 in
      (Pair (e1', e2'), `Product (t1, t2))
  | Fst e -> (
      match elab env e with
      | e', `Product (t, _) -> (Fst e', t)
      | _, t -> expected e.loc (`Product (`Variable "a", `Variable "b")) t)
  | Snd e -> (
      match elab env e with
      | e', `Product (_, t) -> (Snd e', t)
      | _, t -> expected e.loc (`Product (`Variable "a", `Variable "b")) t)
  | Inl (b, e) ->
      let e', a = elab env e in
      (Inl e', `Union (a, b))
  | Inr (a, e) ->
      let e', b = elab env e in
      (Inr e', `Union (a, b))
  | Case (e, (x, s, e1), (y, t, e2)) ->
      let e' = elab_as env e (`Union (s, t)) in
      let e1' = elab_as ((x, s) :: env) e1 s in
      let e2' = elab_as ((y, t) :: env) e2 t in
      (Case (e', (x, e1'), (y, e2')), `Unit)
  | Lambda (x, t, e1) ->
      let e1', t1 = elab ((x, t) :: env) e1 in
      (Lambda (x, e1'), `Arrow (t, t1))
  | App (e1, e2) -> (
      match elab env e1 with
      | e1', `Arrow (a, b) ->
          let e2' = elab_as env e2 a in
          (App (e1', e2'), b)
      | _, t2 -> expected e1.loc (`Arrow (`Variable "a", `Variable "b")) t2)
  | Let (x, t, pe1, pe2) ->
      let ie1 = elab_as env pe1 t in
      let ie2, t2 = elab ((x, t) :: env) pe2 in
      (App (Lambda (x, ie2), ie1), t2)
  | LetFun (f, (x, t1, e2), t2, e) ->
      let e2' = elab_as ((x, t1) :: (f, `Arrow (t1, t2)) :: env) e2 t2 in
      let e', t = elab ((f, `Arrow (t1, t2)) :: env) e in
      (LetRecFun (f, (x, e2'), e'), t)
(* TODO: reimplement free variable check to use LecRec instead of LetRecFun *)
(* let env' = (f, TEarrow (t1, t2)) :: env in let body' = elaborate ((x, t1) ::
    env') body in let bound_vars_except_f = x :: List.filter_map (fun (z, _) ->
    if z = f then None else Some x) env in let make = if List.mem f (fv
    bound_vars_except_f body) then make_letrecfun else make_letfun in make loc f
    x t1 t2 body' (elaborate env' e) *)
