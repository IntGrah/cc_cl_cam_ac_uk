(* Using IntMap to represent memory *)
module IntMap : Map.S with type key = int

type address = int
type var = string

type value
and closure = code * env

and instruction =
  | PUSH of value
  | LOOKUP of var
  | UNARY of Ast.Unary_op.t
  | OPER of Ast.Binary_op.t
  | ASSIGN
  | SWAP
  | POP
  | BIND of var
  | FST
  | SND
  | DEREF
  | APPLY
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of code
  | MK_REC of var * code
  | TEST of code * code
  | CASE of code * code
  | WHILE of code * code

and code = instruction list
and binding = Ast.var * value
and env = binding list

type env_or_value = EV of env | V of value
type env_value_stack = env_or_value list

(* array of referenced values together with next unallocated address *)
type state = value IntMap.t * int
type interp_state = code * env_value_stack * state

val initial_state : state
val initial_env : env_value_stack
val step : interp_state -> interp_state
val compile : Ast.t -> code
val interpret : Ast.t -> value
val pp_instruction : Format.formatter -> instruction -> unit
val pp_value : Format.formatter -> value -> unit
val pp_env_or_value : Format.formatter -> env_or_value -> unit
val pp_code : Format.formatter -> code -> unit
