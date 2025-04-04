type t =
  [ `Int
  | `Bool
  | `Unit
  | `Ref of t
  | `Arrow of t * t
  | `Product of t * t
  | `Union of t * t ]

type hole =
  [ `Int
  | `Bool
  | `Unit
  | `Ref of hole
  | `Arrow of hole * hole
  | `Product of hole * hole
  | `Union of hole * hole
  | `Variable of string ]

let rec to_string = function
  | `Int -> Format.sprintf "Int"
  | `Bool -> Format.sprintf "Bool"
  | `Unit -> Format.sprintf "Unit"
  | `Ref t -> Format.sprintf "Ref(%s)" (to_string t)
  | `Arrow (t1, t2) ->
      Format.sprintf "Arrow(%s, %s)" (to_string t1) (to_string t2)
  | `Product (t1, t2) ->
      Format.sprintf "Product(%s, %s)" (to_string t1) (to_string t2)
  | `Union (t1, t2) ->
      Format.sprintf "Sum(%s, %s)" (to_string t1) (to_string t2)
  | `Variable str -> Format.sprintf "Variable(%s)" str

let rec pp ppf = function
  | `Int -> Format.fprintf ppf "int"
  | `Bool -> Format.fprintf ppf "bool"
  | `Unit -> Format.fprintf ppf "unit"
  | `Ref t -> Format.fprintf ppf "(%a ref)" pp t
  | `Arrow (t1, t2) -> Format.fprintf ppf "(%a -> %a)" pp t1 pp t2
  | `Product (t1, t2) -> Format.fprintf ppf "(%a * %a)" pp t1 pp t2
  | `Union (t1, t2) -> Format.fprintf ppf "(%a + %a)" pp t1 pp t2
  | `Variable str -> Format.fprintf ppf "'%s" str
