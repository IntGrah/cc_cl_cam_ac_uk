type t =
  | Int
  | Bool
  | Unit
  | Ref of t
  | Arrow of t * t
  | Product of t * t
  | Sum of t * t

exception Type_error of { loc : Lexing.position; message : string }

open Format

let rec pp fmt = function
  | Int -> fprintf fmt "Int"
  | Bool -> fprintf fmt "Bool"
  | Unit -> fprintf fmt "Unit"
  | Ref t -> fprintf fmt "Ref(%a)" pp t
  | Arrow (t1, t2) -> fprintf fmt "Arrow(%a, %a)" pp t1 pp t2
  | Product (t1, t2) -> fprintf fmt "Product(%a, %a)" pp t1 pp t2
  | Sum (t1, t2) -> fprintf fmt "Sum(%a, %a)" pp t1 pp t2

let rec pp_nice fmt = function
  | Int -> fprintf fmt "int"
  | Bool -> fprintf fmt "bool"
  | Unit -> fprintf fmt "unit"
  | Ref t -> fprintf fmt "(%a ref)" pp_nice t
  | Arrow (t1, t2) -> fprintf fmt "(%a -> %a)" pp_nice t1 pp_nice t2
  | Product (t1, t2) -> fprintf fmt "(%a * %a)" pp_nice t1 pp_nice t2
  | Sum (t1, t2) -> fprintf fmt "(%a + %a)" pp_nice t1 pp_nice t2
