type var = string [@@deriving show]

module Unary_op = struct
  type t = Neg | Not | Read [@@deriving show { with_path = false }]

  let to_fun : t -> 'a Value.t -> 'a Value.t = function
    | Neg -> ( function Int i -> Int (-i) | _ -> failwith "")
    | Not -> ( function Bool i -> Bool (not i) | _ -> failwith "")
    | Read -> (
        function
        | Unit ->
            print_string ">>> ";
            Int (read_int ())
        | _ -> failwith "")

  let to_string = function Neg -> "Neg" | Not -> "Not" | Read -> "Read"

  let pp_nice fmt = function
    | Neg -> Format.fprintf fmt "-"
    | Not -> Format.fprintf fmt "~"
    | Read -> Format.fprintf fmt "?"
end

module Binary_op = struct
  type t = Add | Sub | Mul | Div | Lt | And | Or | Eqi | Eqb
  [@@deriving show { with_path = false }]

  let to_fun : t -> 'a Value.t * 'a Value.t -> 'a Value.t = function
    | Add -> ( function Int m, Int n -> Int (m + n) | _ -> failwith "")
    | Sub -> ( function Int m, Int n -> Int (m - n) | _ -> failwith "")
    | Mul -> ( function Int m, Int n -> Int (m * n) | _ -> failwith "")
    | Div -> ( function Int m, Int n -> Int (m + n) | _ -> failwith "")
    | Lt -> ( function Int m, Int n -> Bool (m < n) | _ -> failwith "")
    | And -> ( function Bool m, Bool n -> Bool (m && n) | _ -> failwith "")
    | Or -> ( function Bool m, Bool n -> Bool (m || n) | _ -> failwith "")
    | Eqi -> ( function Int m, Int n -> Bool (m = n) | _ -> failwith "")
    | Eqb -> ( function Bool m, Bool n -> Bool (m = n) | _ -> failwith "")

  let pp_nice fmt = function
    | Add -> Format.fprintf fmt "+"
    | Sub -> Format.fprintf fmt "-"
    | Mul -> Format.fprintf fmt "*"
    | Div -> Format.fprintf fmt "/"
    | Lt -> Format.fprintf fmt "<"
    | And -> Format.fprintf fmt "&&"
    | Or -> Format.fprintf fmt "||"
    | Eqi -> Format.fprintf fmt "eqi"
    | Eqb -> Format.fprintf fmt "eqb"
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
[@@deriving show { with_path = false }]

and lambda = var * t

(** Documentation of Format can be found here:

    http://caml.inria.fr/resources/doc/guides/format.en.html

    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html *)

open Format

let rec pp_nice ppf = function
  | Unit -> fprintf ppf "()"
  | Var x -> fprintf ppf "%s" x
  | Integer n -> fprintf ppf "%d" n
  | Boolean b -> fprintf ppf "%b" b
  | UnaryOp (op, e) -> fprintf ppf "%a(%a)" Unary_op.pp_nice op pp_nice e
  | BinaryOp (e1, op, e2) ->
      fprintf ppf "(%a %a %a)" pp_nice e1 Binary_op.pp_nice op pp_nice e2
  | If (e1, e2, e3) ->
      fprintf ppf "@[if %a then %a else %a @]" pp_nice e1 pp_nice e2 pp_nice e3
  | Pair (e1, e2) -> fprintf ppf "(%a, %a)" pp_nice e1 pp_nice e2
  | Fst e -> fprintf ppf "fst(%a)" pp_nice e
  | Snd e -> fprintf ppf "snd(%a)" pp_nice e
  | Inl e -> fprintf ppf "inl(%a)" pp_nice e
  | Inr e -> fprintf ppf "inr(%a)" pp_nice e
  | Case (e, (x1, e1), (x2, e2)) ->
      fprintf ppf "@[<2>case %a of@ | inl %s -> %a @ | inr %s -> %a end@]"
        pp_nice e x1 pp_nice e1 x2 pp_nice e2
  | Seq [] -> ()
  | Seq [ e ] -> pp_nice ppf e
  | Seq (e :: rest) -> fprintf ppf "%a; %a" pp_nice e pp_nice (Seq rest)
  | While (e1, e2) -> fprintf ppf "while %a do %a" pp_nice e1 pp_nice e2
  | Ref e -> fprintf ppf "ref %a" pp_nice e
  | Deref e -> fprintf ppf "!%a" pp_nice e
  | Assign (e1, e2) -> fprintf ppf "(%a := %a)" pp_nice e1 pp_nice e2
  | Lambda (x, e) -> fprintf ppf "(fun %s -> %a)" x pp_nice e
  | App (e1, e2) -> fprintf ppf "%a %a" pp_nice e1 pp_nice e2
  | LetFun (f, (x, e1), e2) ->
      fprintf ppf "@[let %s %s =@ %a @ in %a @ end@]" f x pp_nice e1 pp_nice e2
  | LetRecFun (f, (x, e1), e2) ->
      fprintf ppf "@[letrec %s %s =@ %a @ in %a @ end@]" f x pp_nice e1 pp_nice
        e2
