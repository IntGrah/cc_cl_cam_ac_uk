type address = int
type value

val string_of_value : value -> string
val interpret_top_level : Ast.t -> value
