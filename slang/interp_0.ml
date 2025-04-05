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

type value =
  [ `Ref of address
  | `Int of int
  | `Bool of bool
  | `Unit
  | `Pair of value * value
  | `Inl of value
  | `Inr of value
  | `Fun of value -> store -> value * store ]

and store = address -> value

type env = Ast.var -> value

(* auxiliary functions *)

let rec string_of_value : value -> string = function
  | `Ref a -> Format.sprintf "address(%d)" a
  | `Bool b -> Printf.sprintf "%b" b
  | `Int n -> Printf.sprintf "%d" n
  | `Unit -> "()"
  | `Pair (v1, v2) ->
      Format.sprintf "(%s, %s)" (string_of_value v1) (string_of_value v2)
  | `Inl v -> Format.sprintf "inl(%s)" (string_of_value v)
  | `Inr v -> Format.sprintf "inr(%s)" (string_of_value v)
  | `Fun _ -> "function( ... )"

(* update : (env * binding) -> env
   update : (store * (address * value)) -> store
*)
let update (env, (x, v)) =
 fun y ->
  if x = y then
    v
  else
    env y

let next_address = ref 0

let new_address () =
  let a = !next_address in
  next_address := a + 1;
  a

let do_assign a = function v, store -> (`Unit, update (store, (a, v)))

(*
    interpret : (expr * env * store) -> (value * store)
              : (expr * (var -> value) * address -> value) -> value
*)
let rec interpret (env : env) (e : Ast.t) (store : store) : value * store =
  match e with
  | Unit -> (`Unit, store)
  | Var x -> (env x, store)
  | Integer n -> (`Int n, store)
  | Boolean b -> (`Bool b, store)
  | Seq [] -> (`Unit, store) (* should not be seen ... *)
  | Seq [ e ] -> interpret env e store
  | Seq (e :: rest) ->
      let _, store = interpret env e store in
      interpret env (Seq rest) store
  | While (e1, e2) -> (
      let v, store' = interpret env e1 store in
      match v with
      | `Bool true -> interpret env (Seq [ e2; e ]) store'
      | `Bool false -> (`Unit, store')
      | _ -> Errors.complain "runtime error.  Expecting a boolean!")
  | Ref e ->
      let v, store = interpret env e store in
      let a = new_address () in
      (`Ref a, update (store, (a, v)))
  | Deref e -> (
      let v, store = interpret env e store in
      match v with
      | `Ref a -> (store a, store)
      | _ -> Errors.complain "runtime error.  Expecting an address!")
  | Assign (e1, e2) -> (
      match interpret env e1 store with
      | `Ref a, store -> do_assign a (interpret env e2 store)
      | _ ->
          Errors.complain
            "runtime error : expecting an address on left side of assignment")
  | UnaryOp (op, e) ->
      let v, store' = interpret env e store in
      (Ast.Unary_op.to_fun op v, store')
  | BinaryOp (e1, op, e2) ->
      let v1, store1 = interpret env e1 store in
      let v2, store2 = interpret env e2 store1 in
      (Ast.Binary_op.to_fun op (v1, v2), store2)
  | If (e1, e2, e3) -> (
      let v, store' = interpret env e1 store in
      match v with
      | `Bool true -> interpret env e2 store'
      | `Bool false -> interpret env e3 store'
      | _ -> Errors.complain "runtime error.  Expecting a boolean!")
  | Pair (e1, e2) ->
      let v1, store1 = interpret env e1 store in
      let v2, store2 = interpret env e2 store1 in
      (`Pair (v1, v2), store2)
  | Fst e -> (
      match interpret env e store with
      | `Pair (v1, _), store' -> (v1, store')
      | _ -> Errors.complain "runtime error.  Expecting a pair!")
  | Snd e -> (
      match interpret env e store with
      | `Pair (_, v2), store' -> (v2, store')
      | _ -> Errors.complain "runtime error.  Expecting a pair!")
  | Inl e ->
      let v, store' = interpret env e store in
      (`Inl v, store')
  | Inr e ->
      let v, store' = interpret env e store in
      (`Inr v, store')
  | Case (e, (x1, e1), (x2, e2)) -> (
      let v, store' = interpret env e store in
      match v with
      | `Inl v' -> interpret (update (env, (x1, v'))) e1 store'
      | `Inr v' -> interpret (update (env, (x2, v'))) e2 store'
      | _ -> Errors.complain "runtime error.  Expecting inl or inr!")
  | Lambda (x, e) ->
      (`Fun (fun v s -> interpret (update (env, (x, v))) e s), store)
  | App (e1, e2) -> (
      let v2, store1 = interpret env e2 store in
      let v1, store2 = interpret env e1 store1 in
      match v1 with
      | `Fun f -> f v2 store2
      | _ -> Errors.complain "runtime error.  Expecting a function!")
  | LetFun (f, (x, body), e) ->
      let new_env =
        update
          (env, (f, `Fun (fun v s -> interpret (update (env, (x, v))) body s)))
      in
      interpret new_env e store
  | LetRecFun (f, (x, body), e) ->
      let rec new_env g =
        (* a recursive environment! *)
        if g = f then
          `Fun (fun v s -> interpret (update (new_env, (x, v))) body s)
        else
          env g
      in
      interpret new_env e store

(* env_empty : env *)
let empty_env = fun x -> Errors.complain (x ^ " is not defined!\n")

(* store_empty : env *)
let empty_store =
 fun x -> Errors.complain (string_of_int x ^ " is not allocated!\n")

(* interpret_top_level : expr -> value *)
let interpret_top_level e =
  let v, _ = interpret empty_env e empty_store in
  v
