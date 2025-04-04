type var = string

module Unary_op = struct
  type t = Neg | Not | Read

  let to_fun = function
    | Neg -> ( function `Int i -> `Int (-i) | _ -> failwith "")
    | Not -> ( function `Bool i -> `Bool (not i) | _ -> failwith "")
    | Read -> (
        function
        | `Unit ->
            print_string ">>>";
            `Int (read_int ())
        | _ -> failwith "")

  let to_string = function Neg -> "Neg" | Not -> "Not" | Read -> "Read"

  let pp ppf = function
    | Neg -> Format.fprintf ppf "-"
    | Not -> Format.fprintf ppf "~"
    | Read -> Format.fprintf ppf "read"
end

module Binary_op = struct
  type t = Add | Sub | Mul | Div | Lt | And | Or | Eqi | Eqb

  let to_fun = function
    | Add -> ( function `Int m, `Int n -> `Int (m + n) | _ -> failwith "")
    | Sub -> ( function `Int m, `Int n -> `Int (m - n) | _ -> failwith "")
    | Mul -> ( function `Int m, `Int n -> `Int (m * n) | _ -> failwith "")
    | Div -> ( function `Int m, `Int n -> `Int (m + n) | _ -> failwith "")
    | Lt -> ( function `Int m, `Int n -> `Bool (m < n) | _ -> failwith "")
    | And -> ( function `Bool m, `Bool n -> `Bool (m && n) | _ -> failwith "")
    | Or -> ( function `Bool m, `Bool n -> `Bool (m || n) | _ -> failwith "")
    | Eqi -> ( function `Int m, `Int n -> `Bool (m = n) | _ -> failwith "")
    | Eqb -> ( function `Bool m, `Bool n -> `Bool (m = n) | _ -> failwith "")

  let to_string = function
    | Add -> "Add"
    | Sub -> "Sub"
    | Mul -> "Mul"
    | Div -> "Div"
    | Lt -> "Lt"
    | And -> "And"
    | Or -> "Or"
    | Eqi -> "Eqi"
    | Eqb -> "Eqb"

  let pp ppf = function
    | Add -> Format.fprintf ppf "+"
    | Sub -> Format.fprintf ppf "-"
    | Mul -> Format.fprintf ppf "*"
    | Div -> Format.fprintf ppf "/"
    | Lt -> Format.fprintf ppf "<"
    | And -> Format.fprintf ppf "&&"
    | Or -> Format.fprintf ppf "||"
    | Eqi -> Format.fprintf ppf "eqi"
    | Eqb -> Format.fprintf ppf "eqb"
end

type t =
  | Unit
  | Var of var
  | Integer of int
  | Boolean of bool
  | UnaryOp of Unary_op.t * t
  | BinaryOp of t * Binary_op.t * t
  | If of t * t * t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Inl of t
  | Inr of t
  | Case of t * lambda * lambda
  | While of t * t
  | Seq of t list
  | Ref of t
  | Deref of t
  | Assign of t * t
  | Lambda of lambda
  | App of t * t
  | LetFun of var * lambda * t
  | LetRecFun of var * lambda * t

and lambda = var * t

(** Documentation of Format can be found here:

    http://caml.inria.fr/resources/doc/guides/format.en.html

    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html *)
let rec to_string = function
  | Unit -> "Unit"
  | Var x -> Format.sprintf "Var %s" x
  | Integer n -> Format.sprintf "Integer %d" n
  | Boolean b -> Format.sprintf "Boolean %b" b
  | UnaryOp (op, e) ->
      Format.sprintf "UnaryOp(%s, %s)" (Unary_op.to_string op) (to_string e)
  | BinaryOp (e1, op, e2) ->
      Format.sprintf "Op(%s, %s, %s)" (to_string e1) (Binary_op.to_string op)
        (to_string e2)
  | If (e1, e2, e3) ->
      Format.sprintf "If(%s, %s, %s)" (to_string e1) (to_string e2)
        (to_string e3)
  | Pair (e1, e2) -> Format.sprintf "Pair(%s, %s)" (to_string e1) (to_string e2)
  | Fst e -> Format.sprintf "Fst(%s)" (to_string e)
  | Snd e -> Format.sprintf "Snd(%s)" (to_string e)
  | Inl e -> Format.sprintf "Inl(%s)" (to_string e)
  | Inr e -> Format.sprintf "Inr(%s)" (to_string e)
  | Case (e, (x1, e1), (x2, e2)) ->
      Format.sprintf "Case(%s, (%s, %s), (%s, %s))" (to_string e) x1
        (to_string e1) x2 (to_string e2)
  | While (e1, e2) ->
      Format.sprintf "While(%s, %s)" (to_string e1) (to_string e2)
  | Seq el ->
      Format.sprintf "Seq(%s)" (List.map to_string el |> String.concat "; ")
  | Ref e -> Format.sprintf "Ref(%s)" (to_string e)
  | Deref e -> Format.sprintf "Deref(%s)" (to_string e)
  | Assign (e1, e2) ->
      Format.sprintf "Assign(%s, %s)" (to_string e1) (to_string e2)
  | Lambda (x, e) -> Format.sprintf "Lambda(%s, %s)" x (to_string e)
  | App (e1, e2) -> Format.sprintf "App(%s, %s)" (to_string e1) (to_string e2)
  | LetFun (f, (x, e1), e2) ->
      Format.sprintf "LetFun(%s, (%s, %s), %s)" f x (to_string e1)
        (to_string e2)
  | LetRecFun (f, (x, e1), e2) ->
      Format.sprintf "LetRecFun(%s, (%s, %s), %s)" f x (to_string e1)
        (to_string e2)

let rec pp ppf =
  let open Format in
  function
  | Unit -> fprintf ppf "()"
  | Var x -> fprintf ppf "%s" x
  | Integer n -> fprintf ppf "%d" n
  | Boolean b -> fprintf ppf "%b" b
  | UnaryOp (op, e) -> fprintf ppf "%a(%a)" Unary_op.pp op pp e
  | BinaryOp (e1, op, e2) ->
      fprintf ppf "(%a %a %a)" pp e1 Binary_op.pp op pp e2
  | If (e1, e2, e3) ->
      fprintf ppf "@[if %a then %a else %a @]" pp e1 pp e2 pp e3
  | Pair (e1, e2) -> fprintf ppf "(%a, %a)" pp e1 pp e2
  | Fst e -> fprintf ppf "fst(%a)" pp e
  | Snd e -> fprintf ppf "snd(%a)" pp e
  | Inl e -> fprintf ppf "inl(%a)" pp e
  | Inr e -> fprintf ppf "inr(%a)" pp e
  | Case (e, (x1, e1), (x2, e2)) ->
      fprintf ppf "@[<2>case %a of@ | inl %s -> %a @ | inr %s -> %a end@]" pp e
        x1 pp e1 x2 pp e2
  | Seq [] -> ()
  | Seq [ e ] -> pp ppf e
  | Seq (e :: rest) -> fprintf ppf "%a; %a" pp e pp (Seq rest)
  | While (e1, e2) -> fprintf ppf "while %a do %a" pp e1 pp e2
  | Ref e -> fprintf ppf "ref %a" pp e
  | Deref e -> fprintf ppf "!%a" pp e
  | Assign (e1, e2) -> fprintf ppf "(%a := %a)" pp e1 pp e2
  | Lambda (x, e) -> fprintf ppf "(fun %s -> %a)" x pp e
  | App (e1, e2) -> fprintf ppf "%a %a" pp e1 pp e2
  | LetFun (f, (x, e1), e2) ->
      fprintf ppf "@[let %s %s =@ %a @ in %a @ end@]" f x pp e1 pp e2
  | LetRecFun (f, (x, e1), e2) ->
      fprintf ppf "@[letrec %s %s =@ %a @ in %a @ end@]" f x pp e1 pp e2
