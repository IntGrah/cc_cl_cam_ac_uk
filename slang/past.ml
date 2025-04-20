type var = string

module Loc = struct
  type t = Lexing.position

  let pp ppf (loc : t) =
    Format.fprintf ppf "line %d, position %d" loc.pos_lnum
      (loc.pos_cnum - loc.pos_bol + 1)
end

module Unary_op = struct
  type t = Neg | Not

  let to_string = function Neg -> "Neg" | Not -> "Not"

  let pp ppf = function
    | Neg -> Format.fprintf ppf "-"
    | Not -> Format.fprintf ppf "~"
end

module Binary_op = struct
  type t = Add | Sub | Mul | Div | Lt | And | Or | Eq

  let to_string = function
    | Add -> "Add"
    | Sub -> "Sub"
    | Mul -> "Mul"
    | Div -> "Div"
    | Lt -> "Lt"
    | And -> "And"
    | Or -> "Or"
    | Eq -> "Eq"

  let pp ppf = function
    | Add -> Format.fprintf ppf "+"
    | Sub -> Format.fprintf ppf "-"
    | Mul -> Format.fprintf ppf "*"
    | Div -> Format.fprintf ppf "/"
    | Lt -> Format.fprintf ppf "<"
    | And -> Format.fprintf ppf "&&"
    | Or -> Format.fprintf ppf "||"
    | Eq -> Format.fprintf ppf "="
end

type t = { loc : Loc.t; expr : expr }

and expr =
  | Unit
  | What
  | Var of var
  | Integer of int
  | Boolean of bool
  | UnaryOp of Unary_op.t * t
  | BinaryOp of t * Binary_op.t * t
  | If of t * t * t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Inl of Type.t * t
  | Inr of Type.t * t
  | Case of t * lambda * lambda
  | While of t * t
  | Seq of t list
  | Ref of t
  | Deref of t
  | Assign of t * t
  | Lambda of lambda
  | App of t * t
  | Let of var * Type.t * t * t
  | LetFun of var * lambda * Type.t * t

and lambda = var * Type.t * t

open Format

let rec pp fmt t =
  match t.expr with
  | Unit -> fprintf fmt "Unit"
  | What -> fprintf fmt "What"
  | Var x -> fprintf fmt "Var %s" x
  | Integer n -> fprintf fmt "Integer %d" n
  | Boolean b -> fprintf fmt "Boolean %b" b
  | UnaryOp (op, e) ->
      fprintf fmt "UnaryOp(%s, %a)" (Unary_op.to_string op) pp e
  | BinaryOp (e1, op, e2) ->
      fprintf fmt "Op(%a, %a, %a)" pp e1 Binary_op.pp op pp e2
  | If (e1, e2, e3) -> fprintf fmt "If(%a, %a, %a)" pp e1 pp e2 pp e3
  | Pair (e1, e2) -> fprintf fmt "Pair(%a, %a)" pp e1 pp e2
  | Fst e -> fprintf fmt "Fst(%a)" pp e
  | Snd e -> fprintf fmt "Snd(%a)" pp e
  | Inl (t, e) -> fprintf fmt "Inl(%a, %a)" Type.pp t pp e
  | Inr (t, e) -> fprintf fmt "Inr(%a, %a)" Type.pp t pp e
  | Case (e, (x1, t1, e1), (x2, t2, e2)) ->
      fprintf fmt "Case(%a, (%s, %a, %a), (%s, %a, %a))" pp e x1 Type.pp t1 pp
        e1 x2 Type.pp t2 pp e2
  | While (e1, e2) -> fprintf fmt "While(%a, %a)" pp e1 pp e2
  | Seq el ->
      fprintf fmt "Seq(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp)
        el
  | Ref e -> fprintf fmt "Ref(%a)" pp e
  | Deref e -> fprintf fmt "Deref(%a)" pp e
  | Assign (e1, e2) -> fprintf fmt "Assign(%a, %a)" pp e1 pp e2
  | Lambda (x, t, e) -> fprintf fmt "Lambda(%s, %a, %a)" x Type.pp t pp e
  | App (e1, e2) -> fprintf fmt "App(%a, %a)" pp e1 pp e2
  | Let (x, t, e1, e2) ->
      fprintf fmt "Let(%s, %a, %a, %a)" x Type.pp t pp e1 pp e2
  | LetFun (f, (x, t1, e1), t2, e2) ->
      fprintf fmt "LetFun(%s, (%s, %a, %a), %a, %a)" f x Type.pp t1 pp e1
        Type.pp t2 pp e2

(*
   Documentation of Format can be found here:
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*)

let rec pp_nice ppf t =
  match t.expr with
  | Unit -> fprintf ppf "()"
  | What -> fprintf ppf "?"
  | Var x -> fprintf ppf "%s" x
  | Integer n -> fprintf ppf "%d" n
  | Boolean b -> fprintf ppf "%b" b
  | UnaryOp (op, e) -> fprintf ppf "%a(%a)" Unary_op.pp op pp_nice e
  | BinaryOp (e1, op, e2) ->
      fprintf ppf "(%a %a %a)" pp_nice e1 Binary_op.pp op pp_nice e2
  | If (e1, e2, e3) ->
      fprintf ppf "@[if %a then %a else %a @]" pp_nice e1 pp_nice e2 pp_nice e3
  | Pair (e1, e2) -> fprintf ppf "(%a, %a)" pp_nice e1 pp_nice e2
  | Fst e -> fprintf ppf "fst(%a)" pp_nice e
  | Snd e -> fprintf ppf "snd(%a)" pp_nice e
  | Inl (t, e) -> fprintf ppf "(inl %a %a)" Type.pp t pp_nice e
  | Inr (t, e) -> fprintf ppf "(inr %a %a)" Type.pp t pp_nice e
  | Case (e, (x1, t1, e1), (x2, t2, e2)) ->
      fprintf ppf
        "@[<2>case %a of@ | inl(%s : %a) -> %a @ | inr(%s : %a) -> %a end@]"
        pp_nice e x1 Type.pp t1 pp_nice e1 x2 Type.pp t2 pp_nice e2
  | Seq [] -> ()
  | Seq [ e ] -> pp_nice ppf e
  | Seq (e :: rest) ->
      fprintf ppf "%a; %a" pp_nice e pp_nice { loc = t.loc; expr = Seq rest }
  | While (e1, e2) -> fprintf ppf "while %a do %a" pp_nice e1 pp_nice e2
  | Ref e -> fprintf ppf "ref %a" pp_nice e
  | Deref e -> fprintf ppf "!%a" pp_nice e
  | Assign (e1, e2) -> fprintf ppf "(%a := %a)" pp_nice e1 pp_nice e2
  | Lambda (x, t, e) -> fprintf ppf "(fun %s : %a -> %a)" x Type.pp t pp_nice e
  | App (e1, e2) -> fprintf ppf "%a %a" pp_nice e1 pp_nice e2
  | Let (x, t, e1, e2) ->
      fprintf ppf "@[<2>let %s : %a = %a in %a end@]" x Type.pp t pp_nice e1
        pp_nice e2
  | LetFun (f, (x, t1, e1), t2, e2) ->
      fprintf ppf "@[let %s (%s : %a) : %a =@ %a @ in %a @ end@]" f x Type.pp t1
        Type.pp t2 pp_nice e1 pp_nice e2
