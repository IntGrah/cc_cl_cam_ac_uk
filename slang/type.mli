type t =
  [ `Arrow of t * t
  | `Bool
  | `Int
  | `Product of t * t
  | `Ref of t
  | `Union of t * t
  | `Unit ]

type hole =
  [ `Arrow of hole * hole
  | `Bool
  | `Int
  | `Product of hole * hole
  | `Ref of hole
  | `Union of hole * hole
  | `Unit
  | `Variable of string ]

val to_string :
  ([< `Arrow of 'a * 'a
   | `Bool
   | `Int
   | `Product of 'a * 'a
   | `Ref of 'a
   | `Union of 'a * 'a
   | `Unit
   | `Variable of string ]
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
   | `Union of 'a * 'a
   | `Unit
   | `Variable of string ]
   as
   'a) ->
  unit
