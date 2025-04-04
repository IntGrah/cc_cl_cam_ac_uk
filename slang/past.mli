type var = string

module Loc : sig
  type t = Lexing.position

  val pp : Format.formatter -> t -> unit
end

module Unary_op : sig
  type t = Neg | Not

  val to_string : t -> var
  val pp : Format.formatter -> t -> unit
end

module Binary_op : sig
  type t = Add | Sub | Mul | Div | Lt | And | Or | Eq

  val to_string : t -> var
  val pp : Format.formatter -> t -> unit
end

type t = { loc : Lexing.position; expr : expr }

and expr =
  | Unit
  | What
  | Var of var
  | Integer of int
  | Boolean of bool
  | UnaryOp of Unary_op.t * t
  | BinaryOp of t * Binary_op.t * t
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
