let to_fun = function
  | `Neg -> ( function `Int i -> `Int (-i) | _ -> failwith "")
  | `Not -> ( function `Bool i -> `Bool (not i) | _ -> failwith "")
  | `Read -> (
      function
      | `Unit ->
          print_string "input>";
          `Int (read_int ())
      | _ -> failwith "")

let to_string = function `Neg -> "Neg" | `Not -> "Not" | `Read -> "Read"

let pp ppf = function
  | `Neg -> Format.fprintf ppf "-"
  | `Not -> Format.fprintf ppf "~"
  | `Read -> Format.fprintf ppf "read"
