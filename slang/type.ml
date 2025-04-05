type t =
  [ `Int
  | `Bool
  | `Unit
  | `Ref of t
  | `Arrow of t * t
  | `Product of t * t
  | `Sum of t * t ]

exception Type_error of { loc : Lexing.position; message : string }

let rec to_string = function
  | `Int -> "Int"
  | `Bool -> "Bool"
  | `Unit -> "Unit"
  | `Ref t -> Format.sprintf "Ref(%s)" (to_string t)
  | `Arrow (t1, t2) ->
      Format.sprintf "Arrow(%s, %s)" (to_string t1) (to_string t2)
  | `Product (t1, t2) ->
      Format.sprintf "Product(%s, %s)" (to_string t1) (to_string t2)
  | `Sum (t1, t2) -> Format.sprintf "Sum(%s, %s)" (to_string t1) (to_string t2)
  | `a -> Format.sprintf "'a"
  | `b -> Format.sprintf "'b"
  | `Var str -> str

let rec pp ppf = function
  | `Int -> Format.fprintf ppf "int"
  | `Bool -> Format.fprintf ppf "bool"
  | `Unit -> Format.fprintf ppf "unit"
  | `Ref t -> Format.fprintf ppf "(%a ref)" pp t
  | `Arrow (t1, t2) -> Format.fprintf ppf "(%a -> %a)" pp t1 pp t2
  | `Product (t1, t2) -> Format.fprintf ppf "(%a * %a)" pp t1 pp t2
  | `Sum (t1, t2) -> Format.fprintf ppf "(%a + %a)" pp t1 pp t2
  | `a -> Format.fprintf ppf "'a"
  | `b -> Format.fprintf ppf "'b"
  | `Var str -> Format.fprintf ppf "%s" str
