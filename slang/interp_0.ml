(**************************************
Compiler Construction 2015
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)

(** Interpreter 0 for Slang.2

    This is a "definitional" interpreter for for Slang.2 (the defined language)
    using high-level constructs of OCaml (the defining language). For examples,
    Slang.2 functions are represented as OCaml functions of type

    value -> value

    Slang conditionals are translated to OCaml conditionals, etc. The most
    interesting (and tricky) case is the "let rec" construct of Slang --- this
    is translated using the "lec rec" construct of OCaml. Not with the defined
    function itself, but with the definition of a recursive environment!
    (Because when a recursive function calls itself, it must find its own
    definition in the environment...)

    Note that some of the functions can fail. However, if the input expression
    has passed static analysis, then such "run time" errors should never happen!
    (Can you prove that?) *)

type address = int

type value = (value -> store -> value * store) Value.t
and store = address -> value

let rec pp_value fmt : value -> unit = Value.pp pp_fun fmt
and pp_fun fmt _ = Format.fprintf fmt "Function(...)"

type env = Ast.var -> value

(* Auxiliary functions *)

let update (x, v) env =
 fun y ->
  if x = y then
    v
  else
    env y

let new_address =
  let next_address = ref 0 in
  fun () ->
    let a = !next_address in
    next_address := a + 1;
    a

let rec interp (e : Ast.t) (env : env) (store : store) : value * store =
  (* State monad would be more concise *)
  match e with
  | Unit -> (`Unit, store)
  | Var x -> (env x, store)
  | Integer n -> (`Int n, store)
  | Boolean b -> (`Bool b, store)
  | Seq [] -> (`Unit, store) (* should not be seen ... *)
  | Seq [ e ] -> interp e env store
  | Seq (e :: rest) ->
      let _, store = interp e env store in
      interp (Seq rest) env store
  | While (e1, e2) -> (
      let v, store = interp e1 env store in
      match v with
      | `Bool true -> interp (Seq [ e2; e ]) env store
      | `Bool false -> (`Unit, store)
      | _ -> Errors.complain "Runtime error: expecting a boolean!")
  | Ref e ->
      let v, store = interp e env store in
      let a = new_address () in
      (`Ref a, update (a, v) store)
  | Deref e -> (
      let v, store = interp e env store in
      match v with
      | `Ref a -> (store a, store)
      | _ -> Errors.complain "Runtime error: expecting an address!")
  | Assign (e1, e2) -> (
      match interp e1 env store with
      | `Ref a, store ->
          let v, store = interp e2 env store in
          (`Unit, update (a, v) store)
      | _ ->
          Errors.complain
            "Runtime error: expecting an address on left side of assignment")
  | UnaryOp (op, e) ->
      let v, store = interp e env store in
      (Ast.Unary_op.to_fun op v, store)
  | BinaryOp (e1, op, e2) ->
      let v1, store = interp e1 env store in
      let v2, store = interp e2 env store in
      (Ast.Binary_op.to_fun op (v1, v2), store)
  | If (e1, e2, e3) -> (
      let v, store = interp e1 env store in
      match v with
      | `Bool true -> interp e2 env store
      | `Bool false -> interp e3 env store
      | _ -> Errors.complain "Runtime error: expecting a boolean")
  | Pair (e1, e2) ->
      let v1, store = interp e1 env store in
      let v2, store = interp e2 env store in
      (`Pair (v1, v2), store)
  | Fst e -> (
      match interp e env store with
      | `Pair (v1, _), store -> (v1, store)
      | _ -> Errors.complain "Runtime error: expecting a pair")
  | Snd e -> (
      match interp e env store with
      | `Pair (_, v2), store -> (v2, store)
      | _ -> Errors.complain "Runtime error: expecting a pair")
  | Inl e ->
      let v, store = interp e env store in
      (`Inl v, store)
  | Inr e ->
      let v, store = interp e env store in
      (`Inr v, store)
  | Case (e, (x1, e1), (x2, e2)) -> (
      let v, store = interp e env store in
      match v with
      | `Inl v' -> interp e1 (update (x1, v') env) store
      | `Inr v' -> interp e2 (update (x2, v') env) store
      | _ -> Errors.complain "Runtime error: expecting inl or inr")
  | Lambda (x, e) -> (`Fun (fun v -> interp e (update (x, v) env)), store)
  | App (e1, e2) -> (
      let v2, store = interp e2 env store in
      let v1, store = interp e1 env store in
      match v1 with
      | `Fun f -> f v2 store
      | _ -> Errors.complain "Runtime error: expecting a function")
  | LetFun (f, (x, body), e) ->
      let new_env =
        update (f, `Fun (fun v -> interp body (update (x, v) env))) env
      in
      interp e new_env store
  | LetRecFun (f, (x, body), e) ->
      let rec new_env g =
        (* A recursive environment! *)
        if g = f then
          `Fun (fun v -> interp body (update (x, v) new_env))
        else
          env g
      in
      interp e new_env store

let empty_env : env = fun x -> Errors.complainf "%s is not defined" x
let empty_store : store = fun x -> Errors.complainf "%d is not allocated" x

let interpret (e : Ast.t) : value =
  let v, _ = interp e empty_env empty_store in
  v
