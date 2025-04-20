type code_index = int
type stack_index = int
type heap_index = int
type static_distance = int
type offset = int
type label = string
type location = label * code_index option

type status_code =
  | Halted
  | Running
  | CodeIndexOutOfBound
  | StackIndexOutOfBound
  | HeapIndexOutOfBound
  | StackUnderflow

type stack_item =
  | STACK_INT of int
  | STACK_BOOL of bool
  | STACK_UNIT
  | STACK_HI of heap_index (* Pointer into Heap *)
  | STACK_RA of code_index (* return address *)
  | STACK_FP of stack_index (* Frame pointer *)

type heap_type = HT_PAIR | HT_INL | HT_INR | HT_CLOSURE

type heap_item =
  | HEAP_INT of int
  | HEAP_BOOL of bool
  | HEAP_UNIT
  | HEAP_HI of heap_index (* Pointer into Heap *)
  | HEAP_CI of code_index (* Code pointer for closures *)
  | HEAP_HEADER of int * heap_type (* int is number of items to follow *)

type value_path = STACK_LOCATION of offset | HEAP_LOCATION of offset

type instruction =
  | PUSH of stack_item (* modified *)
  | LOOKUP of value_path (* modified *)
  | UNARY of Ast.Unary_op.t
  | OPER of Ast.Binary_op.t
  | ASSIGN
  | SWAP
  | POP
  (*  | BIND of var            not needed *)
  | FST
  | SND
  | DEREF
  | APPLY
  | RETURN
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of location * int (* modified *)
  | TEST of location
  | CASE of location
  | GOTO of location
  | LABEL of label
  | HALT

type listing = instruction list

type vm_state = {
  stack_bound : stack_index;
  code_bound : code_index;
  heap_bound : code_index;
  stack : stack_item array;
  heap : heap_item array;
  code : instruction array;
  mutable sp : stack_index; (* stack pointer *)
  mutable fp : stack_index; (* frame pointer *)
  mutable cp : code_index; (* code pointer  *)
  mutable hp : heap_index; (* next free     *)
  mutable status : status_code;
}

let get_instruction vm = Array.get vm.code vm.cp
let stack_top vm = Array.get vm.stack (vm.sp - 1)

(********************** Printing ********************************)

let pp_status fmt = function
  | Halted -> Format.fprintf fmt "Halted"
  | Running -> Format.fprintf fmt "Running"
  | CodeIndexOutOfBound -> Format.fprintf fmt "CodeIndexOutOfBound"
  | StackIndexOutOfBound -> Format.fprintf fmt "StackIndexOutOfBound"
  | HeapIndexOutOfBound -> Format.fprintf fmt "HeapIndexOutOfBound"
  | StackUnderflow -> Format.fprintf fmt "StackUnderflow"

let pp_stack_item fmt = function
  | STACK_INT i -> Format.fprintf fmt "STACK_INT %d" i
  | STACK_BOOL true -> Format.fprintf fmt "STACK_BOOL true"
  | STACK_BOOL false -> Format.fprintf fmt "STACK_BOOL false"
  | STACK_UNIT -> Format.fprintf fmt "STACK_UNIT"
  | STACK_HI i -> Format.fprintf fmt "STACK_HI %d" i
  | STACK_RA i -> Format.fprintf fmt "STACK_RA %d" i
  | STACK_FP i -> Format.fprintf fmt "STACK_FP %d" i

let pp_heap_type fmt = function
  | HT_PAIR -> Format.fprintf fmt "HT_PAIR"
  | HT_INL -> Format.fprintf fmt "HT_INL"
  | HT_INR -> Format.fprintf fmt "HT_INR"
  | HT_CLOSURE -> Format.fprintf fmt "HT_CLOSURE"

let pp_heap_item fmt = function
  | HEAP_INT i -> Format.fprintf fmt "HEAP_INT %d" i
  | HEAP_BOOL true -> Format.fprintf fmt "HEAP_BOOL true"
  | HEAP_BOOL false -> Format.fprintf fmt "HEAP_BOOL false"
  | HEAP_UNIT -> Format.fprintf fmt "HEAP_UNIT"
  | HEAP_HI i -> Format.fprintf fmt "HEAP_HI %d" i
  | HEAP_CI i -> Format.fprintf fmt "HEAP_CI %d" i
  | HEAP_HEADER (i, t) ->
      Format.fprintf fmt "HEAP_HEADER(%d, %a)" i pp_heap_type t

let pp_value_path fmt = function
  | STACK_LOCATION offset -> Format.fprintf fmt "STACK_LOCATION %d" offset
  | HEAP_LOCATION offset -> Format.fprintf fmt "HEAP_LOCATION %d" offset

let pp_location fmt = function
  | l, None -> Format.fprintf fmt "%s" l
  | l, Some i -> Format.fprintf fmt "%s = %d" l i

let pp_instruction fmt = function
  | UNARY op -> Format.fprintf fmt "UNARY %a" Ast.Unary_op.pp op
  | OPER op -> Format.fprintf fmt "OPER %a" Ast.Binary_op.pp op
  | MK_PAIR -> Format.fprintf fmt "MK_PAIR"
  | FST -> Format.fprintf fmt "FST"
  | SND -> Format.fprintf fmt "SND"
  | MK_INL -> Format.fprintf fmt "MK_INL"
  | MK_INR -> Format.fprintf fmt "MK_INR"
  | MK_REF -> Format.fprintf fmt "MK_REF"
  | PUSH v -> Format.fprintf fmt "PUSH %a" pp_stack_item v
  | LOOKUP p -> Format.fprintf fmt "LOOKUP %a" pp_value_path p
  | TEST l -> Format.fprintf fmt "TEST %a" pp_location l
  | CASE l -> Format.fprintf fmt "CASE %a" pp_location l
  | GOTO l -> Format.fprintf fmt "GOTO %a" pp_location l
  | APPLY -> Format.fprintf fmt "APPLY"
  | RETURN -> Format.fprintf fmt "RETURN"
  | HALT -> Format.fprintf fmt "HALT"
  | LABEL l -> Format.fprintf fmt "LABEL %s" l
  | SWAP -> Format.fprintf fmt "SWAP"
  | POP -> Format.fprintf fmt "POP"
  | DEREF -> Format.fprintf fmt "DEREF"
  | ASSIGN -> Format.fprintf fmt "ASSIGN"
  | MK_CLOSURE (loc, n) ->
      Format.fprintf fmt "MK_CLOSURE(%a, %d)" pp_location loc n

let rec pp_listing fmt = function
  | [] -> Format.fprintf fmt "\n"
  | LABEL l :: rest -> Format.fprintf fmt "\n%s: %a" l pp_listing rest
  | i :: rest -> Format.fprintf fmt "\n\t%a%a" pp_instruction i pp_listing rest

let pp_installed_code fmt (code, size) =
  let rec aux fmt k =
    if size = k then
      Format.fprintf fmt ""
    else
      Format.fprintf fmt "%d: %a\n%a" k pp_instruction code.(k) aux (k + 1)
  in
  aux fmt 0

let pp_stack fmt (sp, stack) =
  let rec aux fmt j =
    if j < sp then
      Format.fprintf fmt "%d: %a\n%a" j pp_stack_item stack.(j) aux (j + 1)
  in
  aux fmt 0

let pp_heap fmt vm =
  let rec aux fmt k =
    if k < vm.hp then
      Format.fprintf fmt "%d -> %a\n%a" k pp_heap_item vm.heap.(k) aux (k + 1)
  in
  Format.fprintf fmt "Heap = \n%a" aux 0

let pp_state fmt vm =
  Format.fprintf fmt "cp = %d -> %a\nfp = %d\nStack = \n%a@.%a" vm.cp
    pp_instruction (get_instruction vm) vm.fp pp_stack (vm.sp, vm.stack) pp_heap
    vm

(* the following two functions are needed to
   pretty-print heap and stack values
*)
let rec pp_heap_value a fmt vm =
  match Array.get vm.heap a with
  | HEAP_INT i -> Format.fprintf fmt "%d" i
  | HEAP_BOOL true -> Format.fprintf fmt "true"
  | HEAP_BOOL false -> Format.fprintf fmt "false"
  | HEAP_UNIT -> Format.fprintf fmt "()"
  | HEAP_HI i -> Format.fprintf fmt "%a" (pp_heap_value i) vm
  | HEAP_CI _ ->
      Errors.complain
        "string_of_heap_value: expecting value in heap, found code index"
  | HEAP_HEADER (_, ht) -> (
      match ht with
      | HT_PAIR ->
          Format.fprintf fmt "(%a, %a)"
            (pp_heap_value (a + 1))
            vm
            (pp_heap_value (a + 2))
            vm
      | HT_INL -> Format.fprintf fmt "inl(%a)" (pp_heap_value (a + 1)) vm
      | HT_INR -> Format.fprintf fmt "inr(%a)" (pp_heap_value (a + 1)) vm
      | HT_CLOSURE -> Format.fprintf fmt "CLOSURE")

let pp_value fmt vm =
  match stack_top vm with
  | STACK_INT i -> Format.fprintf fmt "%d" i
  | STACK_BOOL true -> Format.fprintf fmt "true"
  | STACK_BOOL false -> Format.fprintf fmt "false"
  | STACK_UNIT -> Format.fprintf fmt "()"
  | STACK_HI a -> Format.fprintf fmt "%a" (pp_heap_value a) vm
  | STACK_RA _ ->
      Errors.complain
        "string_of_value: expecting value on stack top, found code index"
  | STACK_FP _ ->
      Errors.complain
        "string_of_value: expecting value on stack top, found frame pointer"

(***************************** THE MACHINE ********************************)

let readint () =
  let _ = print_string "input> " in
  read_int ()

let stack_to_heap_item = function
  | STACK_INT i -> HEAP_INT i
  | STACK_BOOL b -> HEAP_BOOL b
  | STACK_UNIT -> HEAP_UNIT
  | STACK_HI i -> HEAP_HI i
  | STACK_RA i -> HEAP_CI i
  | STACK_FP _ ->
      Errors.complain "stack_to_heap_item: no frame pointer allowed on heap"

let heap_to_stack_item = function
  | HEAP_INT i -> STACK_INT i
  | HEAP_BOOL b -> STACK_BOOL b
  | HEAP_UNIT -> STACK_UNIT
  | HEAP_HI i -> STACK_HI i
  | HEAP_CI i -> STACK_RA i
  | HEAP_HEADER (_, _) ->
      Errors.complain "heap_to_stack_item : heap header not allowed on stack"

(* cp := cp + 1  *)
let advance_cp vm =
  if vm.cp < vm.code_bound then
    { vm with cp = vm.cp + 1 }
  else
    { vm with status = CodeIndexOutOfBound }

let goto (i, vm) = { vm with cp = i }

(* pop n items from stack *)
let pop (n, vm) =
  if 0 <= vm.sp - n then
    { vm with sp = vm.sp - n }
  else
    { vm with status = StackUnderflow }

let pop_top vm =
  let c = stack_top vm in
  (c, pop (1, vm))

(* pop c onto stack  *)
let push (c, vm) =
  if vm.sp < vm.stack_bound then
    let _ = Array.set vm.stack vm.sp c in
    { vm with sp = vm.sp + 1 }
  else
    { vm with status = StackIndexOutOfBound }

let swap vm =
  let c1, vm1 = pop_top vm in
  let c2, vm2 = pop_top vm1 in
  push (c2, push (c1, vm2))

let do_unary : Ast.Unary_op.t * stack_item -> stack_item = function
  | Not, STACK_BOOL m -> STACK_BOOL (not m)
  | Neg, STACK_INT m -> STACK_INT (-m)
  | Read, STACK_UNIT -> STACK_INT (readint ())
  | op, _ ->
      Errors.complain
        ("do_unary: malformed unary operator: " ^ Ast.Unary_op.to_string op)

let do_oper : Ast.Binary_op.t * stack_item * stack_item -> stack_item = function
  | And, STACK_BOOL m, STACK_BOOL n -> STACK_BOOL (m && n)
  | Or, STACK_BOOL m, STACK_BOOL n -> STACK_BOOL (m || n)
  | Eqb, STACK_BOOL m, STACK_BOOL n -> STACK_BOOL (m = n)
  | Lt, STACK_INT m, STACK_INT n -> STACK_BOOL (m < n)
  | Eqi, STACK_INT m, STACK_INT n -> STACK_BOOL (m = n)
  | Add, STACK_INT m, STACK_INT n -> STACK_INT (m + n)
  | Sub, STACK_INT m, STACK_INT n -> STACK_INT (m - n)
  | Mul, STACK_INT m, STACK_INT n -> STACK_INT (m * n)
  | Div, STACK_INT m, STACK_INT n -> STACK_INT (m / n)
  | op, _, _ ->
      Errors.complainf "do_oper: malformed binary operator: %a" Ast.Binary_op.pp
        op

let perform_op (op, vm) =
  let v_right, vm1 = pop_top vm in
  let v_left, vm2 = pop_top vm1 in
  push (do_oper (op, v_left, v_right), vm2)

let perform_unary (op, vm) =
  let v, vm1 = pop_top vm in
  push (do_unary (op, v), vm1)

(* implement garbage collection!

   This should free up all heap space
   not reachable from the stack.

   Might also increase heap size.

   Result:
   None = no progress
   Some(vm') = progress made, resulting in vm'
*)
let invoke_garbage_collection _ = None

let allocate (n, vm) =
  let hp1 = vm.hp in
  if hp1 + n < vm.heap_bound then
    (hp1, { vm with hp = vm.hp + n })
  else
    match invoke_garbage_collection vm with
    | None -> Errors.complain "allocate : heap exhausted"
    | Some vm2 ->
        if vm2.hp + n < vm2.heap_bound then
          (vm2.hp, { vm2 with hp = vm2.hp + n })
        else
          Errors.complain "allocate : heap exhausted"

let mk_pair vm =
  let v_right, vm1 = pop_top vm in
  let v_left, vm2 = pop_top vm1 in
  let a, vm3 = allocate (3, vm2) in
  let header = HEAP_HEADER (3, HT_PAIR) in
  let _ = Array.set vm.heap a header in
  let _ = Array.set vm.heap (a + 1) (stack_to_heap_item v_left) in
  let _ = Array.set vm.heap (a + 2) (stack_to_heap_item v_right) in
  push (STACK_HI a, vm3)

let do_fst vm =
  let v, vm1 = pop_top vm in
  match v with
  | STACK_HI a -> (
      match vm1.heap.(a) with
      | HEAP_HEADER (_, HT_PAIR) ->
          push (heap_to_stack_item vm.heap.(a + 1), vm1)
      | _ -> Errors.complain "do_fst : unexpectd heap item")
  | _ -> Errors.complain "do_fst : expecting heap pointer on stack"

let do_snd vm =
  let v, vm1 = pop_top vm in
  match v with
  | STACK_HI a -> (
      match vm1.heap.(a) with
      | HEAP_HEADER (_, HT_PAIR) ->
          push (heap_to_stack_item vm.heap.(a + 2), vm1)
      | _ -> Errors.complain "do_snd : unexpectd heap item")
  | _ -> Errors.complain "do_snd : expecting heap pointer on stack"

let mk_inl vm =
  let v, vm1 = pop_top vm in
  let a, vm2 = allocate (2, vm1) in
  let header = HEAP_HEADER (2, HT_INL) in
  let _ = Array.set vm2.heap a header in
  let _ = Array.set vm2.heap (a + 1) (stack_to_heap_item v) in
  push (STACK_HI a, vm2)

let mk_inr vm =
  let v, vm1 = pop_top vm in
  let a, vm2 = allocate (2, vm1) in
  let header = HEAP_HEADER (2, HT_INR) in
  let _ = Array.set vm2.heap a header in
  let _ = Array.set vm2.heap (a + 1) (stack_to_heap_item v) in
  push (STACK_HI a, vm2)

let case (i, vm) =
  let c, vm1 = pop_top vm in
  match c with
  | STACK_HI a -> (
      let vm2 = push (heap_to_stack_item vm.heap.(a + 1), vm1) in
      match vm1.heap.(a) with
      | HEAP_HEADER (_, HT_INR) -> goto (i, vm2)
      | HEAP_HEADER (_, HT_INL) -> advance_cp vm2
      | _ ->
          Errors.complain "case: runtime error, expecting union header in heap")
  | _ ->
      Errors.complain
        "case: runtime error, expecting heap index on top of stack"

let mk_ref vm =
  let v, vm1 = pop_top vm in
  let a, vm2 = allocate (1, vm1) in
  let _ = Array.set vm2.heap a (stack_to_heap_item v) in
  push (STACK_HI a, vm2)

let deref vm =
  let v, vm1 = pop_top vm in
  match v with
  | STACK_HI a -> push (heap_to_stack_item (Array.get vm1.heap a), vm1)
  | _ -> Errors.complain "deref"

let assign vm =
  let c1, vm1 = pop_top vm in
  let c2, _ = pop_top vm1 in
  match c2 with
  | STACK_HI a ->
      if vm.sp < vm.heap_bound then
        let _ = Array.set vm.heap a (stack_to_heap_item c1) in
        push (STACK_UNIT, vm)
      else
        { vm with status = HeapIndexOutOfBound }
  | _ -> Errors.complain "assing: runtime error, expecting heap index on stack"

let test (i, vm) =
  pop
    ( 1,
      if stack_top vm = STACK_BOOL true then
        advance_cp vm
      else
        { vm with cp = i } )

let return vm =
  let current_fp = vm.fp in
  match (vm.stack.(current_fp), vm.stack.(vm.fp + 1)) with
  | STACK_FP saved_fp, STACK_RA k ->
      let return_value = stack_top vm in
      push (return_value, { vm with cp = k; fp = saved_fp; sp = current_fp - 2 })
  | _ -> Errors.complain "return : malformed stack frame"

let fetch fp vm = function
  | STACK_LOCATION offset -> vm.stack.(fp + offset)
  | HEAP_LOCATION offset -> (
      match vm.stack.(fp - 1) with
      | STACK_HI a -> heap_to_stack_item vm.heap.(a + offset + 1)
      | _ -> Errors.complain "search : expecting closure pointer")

let lookup fp vm vlp = push (fetch fp vm vlp, vm)

let mk_closure = function
  | (_, Some i), n, vm ->
      let a, vm1 = allocate (2 + n, vm) in
      let header = HEAP_HEADER (2 + n, HT_CLOSURE) in
      let code_address = HEAP_CI i in
      let _ = vm1.heap.(a) <- header in
      let _ = vm1.heap.(a + 1) <- code_address in
      let rec aux m =
        if m = n then
          ()
        else
          let v = stack_to_heap_item vm1.stack.(vm.sp - (m + 1)) in
          let _ = vm1.heap.(a + m + 2) <- v in
          aux (m + 1)
      in
      let _ = aux 0 in
      let vm2 = pop (n, vm1) in
      push (STACK_HI a, vm2)
  | (_, None), _, _ ->
      Errors.complain "mk_closure : internal error, no address in closure!"

let apply vm =
  match stack_top vm with
  | STACK_HI a -> (
      match vm.heap.(a + 1) with
      | HEAP_CI i ->
          let new_fp = vm.sp in
          let saved_fp = STACK_FP vm.fp in
          let return_index = STACK_RA (vm.cp + 1) in
          let new_vm = { vm with cp = i; fp = new_fp } in
          push (return_index, push (saved_fp, new_vm))
      | _ ->
          Errors.complain "apply: runtime error, expecting code index in heap")
  | _ ->
      Errors.complain
        "apply: runtime error, expecting heap index on top of stack"

let step vm =
  match get_instruction vm with
  | UNARY op -> advance_cp (perform_unary (op, vm))
  | OPER op -> advance_cp (perform_op (op, vm))
  | MK_PAIR -> advance_cp (mk_pair vm)
  | FST -> advance_cp (do_fst vm)
  | SND -> advance_cp (do_snd vm)
  | MK_INL -> advance_cp (mk_inl vm)
  | MK_INR -> advance_cp (mk_inr vm)
  | PUSH c -> advance_cp (push (c, vm))
  | APPLY -> apply vm
  | LOOKUP vp -> advance_cp (lookup vm.fp vm vp)
  | RETURN -> return vm
  | MK_CLOSURE (l, n) -> advance_cp (mk_closure (l, n, vm))
  | SWAP -> advance_cp (swap vm)
  | POP -> advance_cp (pop (1, vm))
  | LABEL _ -> advance_cp vm
  | DEREF -> advance_cp (deref vm)
  | MK_REF -> advance_cp (mk_ref vm)
  | ASSIGN -> advance_cp (assign vm)
  | HALT -> { vm with status = Halted }
  | GOTO (_, Some i) -> goto (i, vm)
  | TEST (_, Some i) -> test (i, vm)
  | CASE (_, Some i) -> case (i, vm)
  | _ -> Errors.complainf "step : bad state = %a" pp_state vm

(* DRIVER *)

let rec driver n vm =
  let _ =
    if Option.verbose then
      Format.printf "========== state %d ==========@.%a@." n pp_state vm
  in
  if vm.status = Running then
    driver (n + 1) (step vm)
  else
    vm

let map_instruction_labels f = function
  | GOTO (lab, _) -> GOTO (lab, Some (f lab))
  | TEST (lab, _) -> TEST (lab, Some (f lab))
  | CASE (lab, _) -> CASE (lab, Some (f lab))
  | MK_CLOSURE ((lab, _), n) -> MK_CLOSURE ((lab, Some (f lab)), n)
  | inst -> inst

let rec find l y =
  match l with
  | [] -> Errors.complain ("Compile.find : " ^ y ^ " is not found")
  | (x, v) :: rest ->
      if x = y then
        v
      else
        find rest y

(* put code listing into an array, associate an array index to each label *)
let load instr_list =
  (* find array index for each label *)
  let mk_label_to_address l =
    let rec aux carry k = function
      | [] -> carry
      | LABEL lab :: rest -> aux ((lab, k) :: carry) (k + 1) rest
      | _ :: rest -> aux carry (k + 1) rest
    in
    aux [] 0 l
  in
  let label_to_address = mk_label_to_address instr_list in
  let locate_instr = map_instruction_labels (find label_to_address) in
  let located_instr_list = List.map locate_instr instr_list in
  let result = Array.of_list located_instr_list in
  (result, Array.length result)

let initial_state l =
  let code_array, c_bound = load l in
  let _ =
    if Option.verbose then
      Format.printf "Installed Code = \n%a" pp_installed_code
        (code_array, c_bound)
  in
  {
    stack_bound = Option.stack_max;
    heap_bound = Option.heap_max;
    code_bound = c_bound;
    stack = Array.make Option.stack_max (STACK_INT 0);
    heap = Array.make Option.heap_max (HEAP_INT 0);
    code = code_array;
    sp = 0;
    fp = 0;
    cp = 0;
    hp = 0;
    status = Running;
  }

let first_frame vm =
  let saved_fp = STACK_FP 0 in
  let return_index = STACK_RA 0 in
  push (return_index, push (saved_fp, vm))

let run l =
  let vm = driver 1 (first_frame (initial_state l)) in
  match vm.status with
  | Halted -> vm
  | status -> Errors.complainf "run : stopped wth status %a" pp_status status

(* COMPILE *)

let label_ref = ref 0

let new_label =
  let get () =
    let v = !label_ref in
    label_ref := !label_ref + 1;
    "L" ^ string_of_int v
  in
  get

(*

Interp 2

 | (APPLY :: ds,  V(CLOSURE (_, (c, env))) :: (V v) :: evs)
    -> (c @ ds, (V v) :: (EV env) :: evs)

Interp 3

 | (APPLY,  V(CLOSURE ((_, Some i), env)) :: (V v) :: evs)
    -> (i, (V v) :: (EV env) :: (RA (cp + 1)) :: evs)


Jargon VM :

     [clsoure    ]
     [arg        ]
        ...

 == APPLY ==>

     [return address]
fp ->[old fp        ]
     [clsoure       ]
     [arg           ]
        ...

*)

let positions l =
  let rec aux k = function
    | [] -> []
    | a :: rest -> (a, k) :: aux (k + 1) rest
  in
  aux 1 l

let rec comp vmap : Ast.t -> listing * listing = function
  | Unit -> ([], [ PUSH STACK_UNIT ])
  | Boolean b -> ([], [ PUSH (STACK_BOOL b) ])
  | Integer n -> ([], [ PUSH (STACK_INT n) ])
  | UnaryOp (op, e) ->
      let defs, c = comp vmap e in
      (defs, c @ [ UNARY op ])
  | BinaryOp (e1, op, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [ OPER op ])
  | Pair (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [ MK_PAIR ])
  | Fst e ->
      let defs, c = comp vmap e in
      (defs, c @ [ FST ])
  | Snd e ->
      let defs, c = comp vmap e in
      (defs, c @ [ SND ])
  | Inl e ->
      let defs, c = comp vmap e in
      (defs, c @ [ MK_INL ])
  | Inr e ->
      let defs, c = comp vmap e in
      (defs, c @ [ MK_INR ])
  | Case (e1, (x1, e2), (x2, e3)) ->
      let inr_label = new_label () in
      let after_inr_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap (Lambda (x1, e2)) in
      let defs3, c3 = comp vmap (Lambda (x2, e3)) in
      ( defs1 @ defs2 @ defs3,
        c1
        @ [ CASE (inr_label, None) ]
        @ c2
        @ [ APPLY; GOTO (after_inr_label, None); LABEL inr_label ]
        @ c3
        @ [ APPLY; LABEL after_inr_label ] )
  | If (e1, e2, e3) ->
      let else_label = new_label () in
      let after_else_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      let defs3, c3 = comp vmap e3 in
      ( defs1 @ defs2 @ defs3,
        c1
        @ [ TEST (else_label, None) ]
        @ c2
        @ [ GOTO (after_else_label, None); LABEL else_label ]
        @ c3 @ [ LABEL after_else_label ] )
  | Seq [] -> ([], [])
  | Seq [ e ] -> comp vmap e
  | Seq (e :: rest) ->
      let defs1, c1 = comp vmap e in
      let defs2, c2 = comp vmap (Seq rest) in
      (defs1 @ defs2, c1 @ [ POP ] @ c2)
  | Ref e ->
      let defs, c = comp vmap e in
      (defs, c @ [ MK_REF ])
  | Deref e ->
      let defs, c = comp vmap e in
      (defs, c @ [ DEREF ])
  | While (e1, e2) ->
      let test_label = new_label () in
      let end_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      ( defs1 @ defs2,
        [ LABEL test_label ] @ c1
        @ [ TEST (end_label, None) ]
        @ c2
        @ [ POP; GOTO (test_label, None); LABEL end_label; PUSH STACK_UNIT ] )
  | Assign (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [ ASSIGN ])
  | App (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c2 @ c1 @ [ APPLY ])
  | Var x -> ([], [ LOOKUP (find vmap x) ])
  | LetFun (f, (x, e1), e2) -> comp vmap (App (Lambda (f, e2), Lambda (x, e1)))
  | Lambda (x, e) -> comp_lambda vmap (None, x, e)
  | LetRecFun (f, (x, e1), e2) ->
      let defs1, c1 = comp vmap (Lambda (f, e2)) in
      let defs2, c2 = comp_lambda vmap (Some f, x, e1) in
      (defs1 @ defs2, c2 @ c1 @ [ APPLY ])

and comp_lambda vmap (f_opt, x, e) =
  let bound_vars = match f_opt with None -> [ x ] | Some f -> [ x; f ] in
  let f = new_label () in
  let f_bind =
    match f_opt with None -> [] | Some f -> [ (f, STACK_LOCATION (-1)) ]
  in
  let x_bind = (x, STACK_LOCATION (-2)) in
  let fvars = Free_vars.free_vars bound_vars e in
  let fetch_fvars = List.map (fun y -> LOOKUP (find vmap y)) fvars in
  let fvar_bind (y, p) = (y, HEAP_LOCATION p) in
  let env_bind = List.map fvar_bind (positions fvars) in
  let new_vmap = x_bind :: (f_bind @ env_bind @ vmap) in
  let defs, c = comp new_vmap e in
  let def = [ LABEL f ] @ c @ [ RETURN ] in
  ( def @ defs,
    List.rev fetch_fvars @ [ MK_CLOSURE ((f, None), List.length fvars) ] )

let compile e =
  let defs, c = comp [] e in
  let result =
    (* body of program @ stop the interpreter @ function definitions *)
    c @ [ HALT ] @ defs
  in

  if Option.verbose then
    Format.printf "Compiled Code = \n%a" pp_listing result;

  result

let interpret e = run (compile e)
let reset = fun _ -> label_ref := 0
