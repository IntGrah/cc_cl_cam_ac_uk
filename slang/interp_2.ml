(**************************************
Compiler Construction 2016
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)

(** Interpreter 2.

    A high-level stack-oriented abstract machine with compiler. What do I mean
    by "high-level"?

    --- Code is still tree-structured.

    --- Complex values are pushed onto value stack.

    --- Slang state (heap) used only for references.

    --- Code is maintained on a code stack.

    --- Program variables contained in code. *)

module Int_map = Map.Make (Int)

type address = int

type value = f Value.t
and f = Closure of closure | Rec_closure of Ast.var * closure
and closure = code * env

and instruction =
  | PUSH of value
  | LOOKUP of Ast.var
  | UNARY of Ast.Unary_op.t
  | OPER of Ast.Binary_op.t
  | ASSIGN
  | SWAP
  | POP
  | BIND of Ast.var
  | FST
  | SND
  | DEREF
  | APPLY
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of code
  | MK_REC of Ast.var * code
  | TEST of code * code
  | CASE of code * code
  | WHILE of code * code

and code = instruction list
and binding = Ast.var * value
and env = binding list [@@deriving show { with_path = false }]

type env_or_value = EV of env | V of value
[@@deriving show { with_path = false }]

type env_value_stack = env_or_value list [@@deriving show { with_path = false }]

(* This is the the slang program state --- that is, values for references *)
(* It is an array of referenced values together with next unallocated address *)
type state = value Int_map.t * int
type interp_state = code * env_value_stack * state

(* Printing *)

let pp_interp_state fmt (c, evs, s) =
  let pp_heap fmt heap =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%d -> %a" k pp_value v in
    Int_map.bindings heap |> List.iter (pp_binding fmt);
    Format.fprintf fmt "@;"
  in
  Format.fprintf fmt
    "@[<v>Code Stack:@;\
     <1 2>%a@;\
     Env/Value Stack:@;\
     <1 2>%a@;\
     Heap (%d items):@;\
     <1 2>%a@;\
     @]"
    pp_code c pp_env_value_stack evs (snd s) pp_heap (fst s)

(* The "MACHINE" *)

(** allocate a new location in the heap and give it value v *)
let allocate (heap, i) v =
  if i < Option.heap_max then
    let heap = Int_map.add i v heap in
    (i, (heap, i + 1))
  else
    Errors.complain "runtime error: heap kaput"

let deref (heap, _) a = Int_map.find a heap

let assign (heap, i) a v =
  let heap = Int_map.add a v heap in
  (heap, i)

let rec search x : env_value_stack -> value = function
  | [] -> Errors.complainf "%s is not defined!" x
  | V _ :: rest -> search x rest
  | EV env :: rest -> (
      match List.assoc_opt x env with None -> search x rest | Some v -> v)

let rec evs_to_env : env_value_stack -> env = function
  | [] -> []
  | V _ :: rest -> evs_to_env rest
  | EV env :: rest -> env @ evs_to_env rest

(** val step : (code * env_value_stack * state) -> (code * env_value_stack *
    state) *)
let step : interp_state -> interp_state = function
  | PUSH v :: ds, evs, s -> (ds, V v :: evs, s)
  | POP :: ds, _ :: evs, s -> (ds, evs, s)
  | SWAP :: ds, e1 :: e2 :: evs, s -> (ds, e2 :: e1 :: evs, s)
  | BIND x :: ds, V v :: evs, s -> (ds, EV [ (x, v) ] :: evs, s)
  | LOOKUP x :: ds, evs, s -> (ds, V (search x evs) :: evs, s)
  | UNARY op :: ds, V v :: evs, s -> (ds, V (Ast.Unary_op.to_fun op v) :: evs, s)
  | OPER op :: ds, V v2 :: V v1 :: evs, s ->
      (ds, V (Ast.Binary_op.to_fun op (v1, v2)) :: evs, s)
  | MK_PAIR :: ds, V v2 :: V v1 :: evs, s -> (ds, V (Pair (v1, v2)) :: evs, s)
  | FST :: ds, V (Pair (v, _)) :: evs, s -> (ds, V v :: evs, s)
  | SND :: ds, V (Pair (_, v)) :: evs, s -> (ds, V v :: evs, s)
  | MK_INL :: ds, V v :: evs, s -> (ds, V (Inl v) :: evs, s)
  | MK_INR :: ds, V v :: evs, s -> (ds, V (Inr v) :: evs, s)
  | CASE (c1, _) :: ds, V (Inl v) :: evs, s -> (c1 @ ds, V v :: evs, s)
  | CASE (_, c2) :: ds, V (Inr v) :: evs, s -> (c2 @ ds, V v :: evs, s)
  | TEST (c1, _) :: ds, V (Bool true) :: evs, s -> (c1 @ ds, evs, s)
  | TEST (_, c2) :: ds, V (Bool false) :: evs, s -> (c2 @ ds, evs, s)
  | ASSIGN :: ds, V v :: V (Ref a) :: evs, s -> (ds, V Unit :: evs, assign s a v)
  | DEREF :: ds, V (Ref a) :: evs, s -> (ds, V (deref s a) :: evs, s)
  | MK_REF :: ds, V v :: evs, s ->
      let a, s' = allocate s v in
      (ds, V (Ref a) :: evs, s')
  | WHILE (_, _) :: ds, V (Bool false) :: evs, s -> (ds, V Unit :: evs, s)
  | WHILE (c1, c2) :: ds, V (Bool true) :: evs, s ->
      (c2 @ [ POP ] @ c1 @ [ WHILE (c1, c2) ] @ ds, evs, s)
  | MK_CLOSURE c :: ds, evs, s ->
      (ds, V (Fun (Closure (c, evs_to_env evs))) :: evs, s)
  | MK_REC (f, c) :: ds, evs, s ->
      (ds, V (Fun (Rec_closure (f, (c, evs_to_env evs)))) :: evs, s)
  | APPLY :: ds, V (Fun (Closure (c, env))) :: V v :: evs, s ->
      (c @ ds, V v :: EV env :: evs, s)
  | APPLY :: ds, V (Fun (Rec_closure (f, (c, env)))) :: V v :: evs, s ->
      ( APPLY :: ds,
        V (Fun (Closure (c, (f, Fun (Rec_closure (f, (c, env)))) :: env)))
        :: V v :: evs,
        s )
  | state -> Errors.complainf "step : bad state = %a" pp_interp_state state

let rec driver n state =
  if Option.verbose then
    Format.printf "State %d: %a@." n pp_interp_state state;

  match state with [], [ V v ], _ -> v | _ -> driver (n + 1) (step state)

(* A BIND will leave an env on stack.
   This gets rid of it.  *)
let leave_scope = [ SWAP; POP ]

let rec compile : Ast.t -> code = function
  | Unit -> [ PUSH Unit ]
  | Integer n -> [ PUSH (Int n) ]
  | Boolean b -> [ PUSH (Bool b) ]
  | Var x -> [ LOOKUP x ]
  | UnaryOp (op, e) -> compile e @ [ UNARY op ]
  | BinaryOp (e1, op, e2) -> compile e1 @ compile e2 @ [ OPER op ]
  | Pair (e1, e2) -> compile e1 @ compile e2 @ [ MK_PAIR ]
  | Fst e -> compile e @ [ FST ]
  | Snd e -> compile e @ [ SND ]
  | Inl e -> compile e @ [ MK_INL ]
  | Inr e -> compile e @ [ MK_INR ]
  | Case (e, (x1, e1), (x2, e2)) ->
      compile e
      @ [
          CASE
            ( (BIND x1 :: compile e1) @ leave_scope,
              (BIND x2 :: compile e2) @ leave_scope );
        ]
  | If (e1, e2, e3) -> compile e1 @ [ TEST (compile e2, compile e3) ]
  | Seq [] -> []
  | Seq [ e ] -> compile e
  (* Locations on sequence should highlight entire code blocks? *)
  | Seq (e :: rest) -> compile e @ [ POP ] @ compile (Seq rest)
  | Ref e -> compile e @ [ MK_REF ]
  | Deref e -> compile e @ [ DEREF ]
  | While (e1, e2) ->
      let cl = compile e1 in
      cl @ [ WHILE (cl, compile e2) ]
  | Assign (e1, e2) -> compile e1 @ compile e2 @ [ ASSIGN ]
  | App (e1, e2) ->
      compile e2 (* I chose to evaluate arg first *)
      @ compile e1 @ [ APPLY; SWAP; POP ]
      (* get rid of env left on stack *)
  | Lambda (x, e) -> [ MK_CLOSURE ((BIND x :: compile e) @ leave_scope) ]
  | LetFun (f, (x, body), e) ->
      MK_CLOSURE ((BIND x :: compile body) @ leave_scope)
      :: BIND f :: compile e
      @ leave_scope
  | LetRecFun (f, (x, body), e) ->
      (MK_REC (f, (BIND x :: compile body) @ leave_scope) :: BIND f :: compile e)
      @ leave_scope

let interpret (e : Ast.t) : value =
  let c = compile e in
  if Option.verbose then
    Format.printf "Compile code =@\n%a@." pp_code c;

  (* The initial Slang state is the Slang state : all locations contain 0 *)
  let initial_env = [] in
  let initial_state = (Int_map.empty, 0) in

  driver 1 (c, initial_env, initial_state)
