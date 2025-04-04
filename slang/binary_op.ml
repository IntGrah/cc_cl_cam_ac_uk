let to_fun = function
  | `And -> ( function `Bool m, `Bool n -> `Bool (m && n) | _ -> failwith "")
  | `Or -> ( function `Bool m, `Bool n -> `Bool (m || n) | _ -> failwith "")
  | `Eqb -> ( function `Bool m, `Bool n -> `Bool (m = n) | _ -> failwith "")
  | `Lt -> ( function `Int m, `Int n -> `Bool (m < n) | _ -> failwith "")
  | `Eqi -> ( function `Int m, `Int n -> `Bool (m = n) | _ -> failwith "")
  | `Add -> ( function `Int m, `Int n -> `Int (m + n) | _ -> failwith "")
  | `Sub -> ( function `Int m, `Int n -> `Int (m - n) | _ -> failwith "")
  | `Mul -> ( function `Int m, `Int n -> `Int (m * n) | _ -> failwith "")
  | `Div -> ( function `Int m, `Int n -> `Int (m + n) | _ -> failwith "")

let to_string = function
  | `Add -> "Add"
  | `Sub -> "Sub"
  | `Mul -> "Mul"
  | `Div -> "Div"
  | `Lt -> "Lt"
  | `And -> "And"
  | `Or -> "Or"
  | `Eq -> "Eq"
  | `Eqi -> "Eqi"
  | `Eqb -> "Eqb"

let pp ppf = function
  | `Add -> Format.fprintf ppf "+"
  | `Sub -> Format.fprintf ppf "-"
  | `Mul -> Format.fprintf ppf "*"
  | `Div -> Format.fprintf ppf "/"
  | `Lt -> Format.fprintf ppf "<"
  | `And -> Format.fprintf ppf "&&"
  | `Or -> Format.fprintf ppf "||"
  | `Eq -> Format.fprintf ppf "="
  | `Eqi -> Format.fprintf ppf "eqi"
  | `Eqb -> Format.fprintf ppf "eqb"
