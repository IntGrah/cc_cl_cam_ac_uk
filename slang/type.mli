type t =
  | Int
  | Bool
  | Unit
  | Ref of t
  | Arrow of t * t
  | Product of t * t
  | Sum of t * t

exception Type_error of { loc : Lexing.position; message : string }

val pp : Format.formatter -> t -> unit
val pp_nice : Format.formatter -> t -> unit
