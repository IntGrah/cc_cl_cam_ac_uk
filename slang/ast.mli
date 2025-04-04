type var = string
type unary_op = [ `Neg | `Not | `Read ]
type binary_op = [ `Add | `And | `Div | `Eqb | `Eqi | `Lt | `Mul | `Or | `Sub ]

type t =
  | Unit
  | Var of var
  | Integer of int
  | Boolean of bool
  | UnaryOp of unary_op * t
  | BinaryOp of t * binary_op * t
  | If of t * t * t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Inl of t
  | Inr of t
  | Case of t * lambda * lambda
  | While of t * t
  | Seq of t list
  | Ref of t
  | Deref of t
  | Assign of t * t
  | Lambda of lambda
  | App of t * t
  | LetFun of var * lambda * t
  | LetRecFun of var * lambda * t

and lambda = var * t

val to_string : t -> string
val pp : Format.formatter -> t -> unit
