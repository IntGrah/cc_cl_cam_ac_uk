type var = string

module Loc : sig
  type t = Lexing.position

  val pp : Format.formatter -> t -> unit
end

type unary_op = [ `Neg | `Not ]
type binary_op = [ `Add | `And | `Div | `Eq | `Lt | `Mul | `Or | `Sub ]

type t = { loc : Lexing.position; expr : expr }

and expr =
  | Unit
  | What
  | Var of var
  | Integer of int
  | Boolean of bool
  | UnaryOp of unary_op * t
  | BinaryOp of t * binary_op * t
  | If of t * t * t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Inl of Type.t * t
  | Inr of Type.t * t
  | Case of t * lambda * lambda
  | While of t * t
  | Seq of t list
  | Ref of t
  | Deref of t
  | Assign of t * t
  | Lambda of lambda
  | App of t * t
  | Let of var * Type.t * t * t
  | LetFun of var * lambda * Type.t * t

and lambda = var * Type.t * t

val to_string : t -> string
val pp : Format.formatter -> t -> unit
