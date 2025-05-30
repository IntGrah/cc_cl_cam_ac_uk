type code_index = int
type stack_index = int
type heap_index = int
type offset = int
type label = string
type location = label * code_index option

type status_code =
  | Halted
  | Running
  | Code_index_out_of_bounds
  | Stack_index_out_of_bounds
  | Heap_index_out_of_bounds
  | Stack_underflow

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

type vm_state = {
  stack_bound : stack_index;
  code_bound : code_index;
  heap_bound : code_index;
  stack : stack_item array;
  heap : heap_item array;
  code : instruction array;
  sp : stack_index; (* stack pointer *)
  fp : stack_index; (* frame pointer *)
  cp : code_index; (* code pointer *)
  hp : heap_index; (* next free *)
  status : status_code;
}

val new_label : unit -> string
val step : vm_state -> vm_state
val driver : int -> vm_state -> vm_state

type code = instruction list

val comp : (Past.var * value_path) list -> Ast.t -> code * code
val compile : Ast.t -> code
val run : code -> vm_state
val interpret : Ast.t -> vm_state
val pp_stack_item : Format.formatter -> stack_item -> unit
val pp_heap_item : Format.formatter -> heap_item -> unit
val pp_status_code : Format.formatter -> status_code -> unit
val pp_instruction : Format.formatter -> instruction -> unit
val initial_state : code -> vm_state
val first_frame : vm_state -> vm_state
val pp_location : Format.formatter -> location -> unit
val pp_value_path : Format.formatter -> value_path -> unit
val pp_value : Format.formatter -> vm_state -> unit
val reset : unit -> unit
val pp_code : Format.formatter -> code -> unit
