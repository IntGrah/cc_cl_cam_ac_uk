val to_fun :
  [< `Add | `And | `Div | `Eqb | `Eqi | `Lt | `Mul | `Or | `Sub ] ->
  [> `Bool of bool | `Int of int ] * [> `Bool of bool | `Int of int ] ->
  [> `Bool of bool | `Int of int ]

val to_string :
  [< `Add | `And | `Div | `Eq | `Eqb | `Eqi | `Lt | `Mul | `Or | `Sub ] ->
  string

val pp :
  Format.formatter ->
  [< `Add | `And | `Div | `Eq | `Eqb | `Eqi | `Lt | `Mul | `Or | `Sub ] ->
  unit
