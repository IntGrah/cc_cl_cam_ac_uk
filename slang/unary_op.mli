val to_fun :
  [< `Neg | `Not | `Read ] ->
  [> `Bool of bool | `Int of int | `Unit ] ->
  [> `Bool of bool | `Int of int ]

val to_string : [< `Neg | `Not | `Read ] -> string
val pp : Format.formatter -> [< `Neg | `Not | `Read ] -> unit
