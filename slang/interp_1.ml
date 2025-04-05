(**************************************
Compiler Construction 2020
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)
(* Interpreter 1.

   Derived from Interpreter 1 via
   CPS and DFC transformations applied
   to the code of Interp_0.interpret.

*)

type address = int

type value =
  [ `Ref of address
  | `Int of int
  | `Bool of bool
  | `Unit
  | `Pair of value * value
  | `Inl of value
  | `Inr of value
  | `Rec_closure of closure
  | `Closure of closure ]

and closure = Ast.var * Ast.t * env

and continuation_action =
  | UNARY of Ast.Unary_op.t
  | OPER of Ast.Binary_op.t * value
  | OPER_FST of Ast.t * env * Ast.Binary_op.t
  | ASSIGN of value
  | ASSIGN_FST of Ast.t * env
  | TAIL of Ast.t list * env
  | IF of Ast.t * Ast.t * env
  | WHILE of Ast.t * Ast.t * env
  | MKPAIR of value
  | PAIR_FST of Ast.t * env
  | FST
  | SND
  | MKINL
  | MKINR
  | MKREF
  | DEREF
  | CASE of Ast.var * Ast.t * Ast.var * Ast.t * env
  | APPLY of value
  | ARG of Ast.t * env

and continuation = continuation_action list
and binding = Ast.var * value
and env = binding list

type state =
  | INSPECT of Ast.t * env * continuation
  | COMPUTE of continuation * value

(* update : (env * binding) -> env *)
let update (env, (x, v)) = (x, v) :: env

(* When making a closure, only include bindings that
   are needed.
*)

let filter_env fvars = List.filter (fun (x, _) -> List.mem x fvars)

let mk_fun (x, body, env) =
  let fvars = Free_vars.free_vars [ x ] body in
  let smaller_env = filter_env fvars env in
  `Closure (x, body, smaller_env)

let mk_rec_fun (f, x, body, env) =
  let fvars = Free_vars.free_vars [ f; x ] body in
  let smaller_env = filter_env fvars env in
  let f_binding = (f, `Rec_closure (x, body, [])) in
  `Closure (x, body, f_binding :: smaller_env)

(*
      for a recursive function f we want

      lookup (env, f) = FUN(true, (x, body, env))
*)
let lookup (env, x) =
  let rec aux = function
    | [] -> Errors.complain (x ^ " is not defined!\n")
    | (y, v) :: rest ->
        if x = y then
          match v with
          | `Rec_closure (z, body, _) ->
              `Closure (z, body, (y, `Rec_closure (z, body, [])) :: rest)
          | _ -> v
        else
          aux rest
  in
  aux env

let string_of_list sep f l = "[" ^ String.concat sep (List.map f l) ^ "]"

let rec string_of_value : value -> string = function
  | `Ref a -> "REF(" ^ string_of_int a ^ ")"
  | `Bool b -> string_of_bool b
  | `Int n -> string_of_int n
  | `Unit -> "UNIT"
  | `Pair (v1, v2) ->
      "PAIR(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
  | `Inl v -> "INL(" ^ string_of_value v ^ ")"
  | `Inr v -> "INR(" ^ string_of_value v ^ ")"
  | `Closure cl -> "CLOSURE(" ^ string_of_closure cl ^ ")"
  | `Rec_closure cl -> "REC_CLOSURE(" ^ string_of_closure cl ^ ")"

and string_of_closure (x, e, env) =
  x ^ ", " ^ Ast.to_string e ^ ", " ^ string_of_env env

and string_of_env env = string_of_list ",\n " string_of_binding env
and string_of_binding (x, v) = "(" ^ x ^ ", " ^ string_of_value v ^ ")"

let string_of_expr_list = string_of_list "; " Ast.to_string

let string_of_continuation_action = function
  | UNARY op -> "UNARY " ^ Ast.Unary_op.to_string op
  | MKPAIR v -> "MKPAIR " ^ string_of_value v
  | FST -> "FST"
  | SND -> "SND"
  | MKINL -> "MKINL"
  | MKINR -> "MKINR"
  | APPLY v -> "APPLY " ^ string_of_value v
  | ARG (e, env) -> "ARG(" ^ Ast.to_string e ^ ", " ^ string_of_env env ^ ")"
  | OPER (op, v) ->
      "OPER(" ^ Ast.Binary_op.to_string op ^ ", " ^ string_of_value v ^ ")"
  | CASE (x1, e1, x2, e2, env) ->
      "CASE(" ^ x1 ^ ", " ^ Ast.to_string e1 ^ ", " ^ x2 ^ ", "
      ^ Ast.to_string e2 ^ ", " ^ string_of_env env ^ ")"
  | PAIR_FST (e, env) ->
      "PAIR_FST(" ^ Ast.to_string e ^ ", " ^ string_of_env env ^ ")"
  | OPER_FST (e, env, op) ->
      "OPER_FST(" ^ Ast.to_string e ^ ", " ^ string_of_env env ^ ", "
      ^ Ast.Binary_op.to_string op ^ ")"
  | IF (e1, e2, env) ->
      "IF(" ^ Ast.to_string e1 ^ ", " ^ Ast.to_string e2 ^ ", "
      ^ string_of_env env ^ ")"
  | ASSIGN v -> "MKPAIR " ^ string_of_value v
  | ASSIGN_FST (e, env) ->
      "ASSIGN_FST(" ^ Ast.to_string e ^ ", " ^ string_of_env env ^ ")"
  | TAIL (el, env) ->
      "TAIL(" ^ string_of_expr_list el ^ ", " ^ string_of_env env ^ ")"
  | WHILE (e1, e2, env) ->
      "WHILE(" ^ Ast.to_string e1 ^ ", " ^ Ast.to_string e2 ^ ", "
      ^ string_of_env env ^ ")"
  | MKREF -> "MKREF"
  | DEREF -> "DEREF"

let string_of_continuation = string_of_list ";\n " string_of_continuation_action

let string_of_state = function
  | INSPECT (e, env, cnt) ->
      "INSPECT(" ^ Ast.to_string e ^ ", " ^ string_of_env env ^ ", "
      ^ string_of_continuation cnt ^ ")"
  | COMPUTE (cnt, v) ->
      "COMPUTE(" ^ string_of_continuation cnt ^ ", " ^ string_of_value v ^ ")"

let heap = Array.make Option.heap_max (`Int 0)

let new_address =
  let next_address = ref 0 in
  fun () ->
    let a = !next_address in
    next_address := a + 1;
    a

let mk_ref v =
  let a = new_address () in
  heap.(a) <- v;
  `Ref a

let do_assign a v = heap.(a) <- v

let step = function
  (* INSPECT --> INSPECT *)
  | INSPECT (UnaryOp (op, e), env, k) -> INSPECT (e, env, UNARY op :: k)
  | INSPECT (BinaryOp (e1, op, e2), env, k) ->
      INSPECT (e1, env, OPER_FST (e2, env, op) :: k)
  | INSPECT (If (e1, e2, e3), env, k) -> INSPECT (e1, env, IF (e2, e3, env) :: k)
  | INSPECT (Pair (e1, e2), env, k) -> INSPECT (e1, env, PAIR_FST (e2, env) :: k)
  | INSPECT (Fst e, env, k) -> INSPECT (e, env, FST :: k)
  | INSPECT (Snd e, env, k) -> INSPECT (e, env, SND :: k)
  | INSPECT (Inl e, env, k) -> INSPECT (e, env, MKINL :: k)
  | INSPECT (Inr e, env, k) -> INSPECT (e, env, MKINR :: k)
  | INSPECT (Case (e, (x1, e1), (x2, e2)), env, k) ->
      INSPECT (e, env, CASE (x1, e1, x2, e2, env) :: k)
  | INSPECT (App (e1, e2), env, k) -> INSPECT (e2, env, ARG (e1, env) :: k)
  | INSPECT (LetFun (f, (x, body), e), env, k) ->
      INSPECT (e, update (env, (f, mk_fun (x, body, env))), k)
  | INSPECT (LetRecFun (f, (x, body), e), env, k) ->
      INSPECT (e, update (env, (f, mk_rec_fun (f, x, body, env))), k)
  | INSPECT (Ref e, env, k) -> INSPECT (e, env, MKREF :: k)
  | INSPECT (Deref e, env, k) -> INSPECT (e, env, DEREF :: k)
  | INSPECT (Assign (e1, e2), env, k) ->
      INSPECT (e1, env, ASSIGN_FST (e2, env) :: k)
  | INSPECT (Seq [ e ], env, k) -> INSPECT (e, env, k)
  | INSPECT (Seq (e :: rest), env, k) -> INSPECT (e, env, TAIL (rest, env) :: k)
  | INSPECT (While (e1, e2), env, k) ->
      INSPECT (e1, env, WHILE (e1, e2, env) :: k)
  (* INSPECT --> COMPUTE *)
  | INSPECT (Unit, _, k) -> COMPUTE (k, `Unit)
  | INSPECT (Var x, env, k) -> COMPUTE (k, lookup (env, x))
  | INSPECT (Integer n, _, k) -> COMPUTE (k, `Int n)
  | INSPECT (Boolean b, _, k) -> COMPUTE (k, `Bool b)
  | INSPECT (Lambda (x, body), env, k) -> COMPUTE (k, mk_fun (x, body, env))
  (* COMPUTE --> COMPUTE *)
  | COMPUTE (UNARY op :: k, v) -> COMPUTE (k, Ast.Unary_op.to_fun op v)
  | COMPUTE (OPER (op, v1) :: k, v2) ->
      COMPUTE (k, Ast.Binary_op.to_fun op (v1, v2))
  | COMPUTE (MKPAIR v1 :: k, v2) -> COMPUTE (k, `Pair (v1, v2))
  | COMPUTE (FST :: k, `Pair (v, _)) -> COMPUTE (k, v)
  | COMPUTE (SND :: k, `Pair (_, v)) -> COMPUTE (k, v)
  | COMPUTE (MKINL :: k, v) -> COMPUTE (k, `Inl v)
  | COMPUTE (MKINR :: k, v) -> COMPUTE (k, `Inr v)
  | COMPUTE (MKREF :: k, v) -> COMPUTE (k, mk_ref v)
  | COMPUTE (DEREF :: k, `Ref a) -> COMPUTE (k, heap.(a))
  | COMPUTE (ASSIGN (`Ref a) :: k, v) ->
      let _ = do_assign a v in
      COMPUTE (k, `Unit)
  | COMPUTE (WHILE (_, _, _) :: k, `Bool false) -> COMPUTE (k, `Unit)
  (* COMPUTE --> INSPECT *)
  | COMPUTE (OPER_FST (e2, env, op) :: k, v1) ->
      INSPECT (e2, env, OPER (op, v1) :: k)
  | COMPUTE (APPLY v2 :: k, `Closure (x, body, env)) ->
      INSPECT (body, update (env, (x, v2)), k)
  | COMPUTE (APPLY v2 :: k, `Rec_closure (x, body, env)) ->
      INSPECT (body, update (env, (x, v2)), k)
  | COMPUTE (ARG (e2, env) :: k, v) -> INSPECT (e2, env, APPLY v :: k)
  | COMPUTE (PAIR_FST (e2, env) :: k, v1) -> INSPECT (e2, env, MKPAIR v1 :: k)
  | COMPUTE (CASE (x1, e1, _, _, env) :: k, `Inl v) ->
      INSPECT (e1, update (env, (x1, v)), k)
  | COMPUTE (CASE (_, _, x2, e2, env) :: k, `Inr v) ->
      INSPECT (e2, update (env, (x2, v)), k)
  | COMPUTE (IF (e2, _, env) :: k, `Bool true) -> INSPECT (e2, env, k)
  | COMPUTE (IF (_, e3, env) :: k, `Bool false) -> INSPECT (e3, env, k)
  | COMPUTE (ASSIGN_FST (e2, env) :: k, v) -> INSPECT (e2, env, ASSIGN v :: k)
  | COMPUTE (WHILE (e1, e2, env) :: k, `Bool true) ->
      INSPECT (Seq [ e2; e1 ], env, WHILE (e1, e2, env) :: k)
  | COMPUTE (TAIL (el, env) :: k, _) -> INSPECT (Seq el, env, k)
  | state ->
      Errors.complain
        ("step : malformed state = " ^ string_of_state state ^ "\n")

let rec driver n state =
  if Option.verbose then
    print_string
      ("\nstate " ^ string_of_int n ^ " = \n" ^ string_of_state state ^ "\n");
  match state with COMPUTE ([], v) -> v | _ -> driver (n + 1) (step state)

let eval (e, env) = driver 1 (INSPECT (e, env, []))
let env_empty : env = []
let interpret (e : Ast.t) : value = eval (e, env_empty)
