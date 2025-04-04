exception
  Type_error of { loc : Past.Loc.t; expecting : Type.hole; found : Type.t }

type env = (Past.var * Type.t) list

val elab : env -> Past.t -> Ast.t * Type.t
