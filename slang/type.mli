type t =
  [ `Arrow of t * t
  | `Bool
  | `Int
  | `Product of t * t
  | `Ref of t
  | `Sum of t * t
  | `Unit ]

exception Type_error of { loc : Lexing.position; message : string }

val to_string :
  ([< `Arrow of 'a * 'a
   | `Bool
   | `Int
   | `Product of 'a * 'a
   | `Ref of 'a
   | `Sum of 'a * 'a
   | `Unit
   | `a
   | `b
   | `Var of string ]
   as
   'a) ->
  string

val pp :
  Format.formatter ->
  ([< `Arrow of 'a * 'a
   | `Bool
   | `Int
   | `Product of 'a * 'a
   | `Ref of 'a
   | `Sum of 'a * 'a
   | `Unit
   | `a
   | `b
   | `Var of string ]
   as
   'a) ->
  unit
