type var = string [@@deriving show]

module Loc = struct
  type t = Lexing.position

  let pp ppf (loc : t) =
    Format.fprintf ppf "line %d, position %d" loc.pos_lnum
      (loc.pos_cnum - loc.pos_bol + 1)
end

module Unary_op = struct
  type t = Neg | Not [@@deriving show { with_path = false }]

  let pp_nice fmt = function
    | Neg -> Format.fprintf fmt "-"
    | Not -> Format.fprintf fmt "~"
end

module Binary_op = struct
  type t = Add | Sub | Mul | Div | Lt | And | Or | Eq
  [@@deriving show { with_path = false }]

  let pp_nice fmt = function
    | Add -> Format.fprintf fmt "+"
    | Sub -> Format.fprintf fmt "-"
    | Mul -> Format.fprintf fmt "*"
    | Div -> Format.fprintf fmt "/"
    | Lt -> Format.fprintf fmt "<"
    | And -> Format.fprintf fmt "&&"
    | Or -> Format.fprintf fmt "||"
    | Eq -> Format.fprintf fmt "="
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

and lambda = var * Type.t * t [@@deriving show { with_path = false }]

open Format

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
  | UnaryOp (op, e) -> fprintf ppf "%a(%a)" Unary_op.pp_nice op pp_nice e
  | BinaryOp (e1, op, e2) ->
      fprintf ppf "(%a %a %a)" pp_nice e1 Binary_op.pp_nice op pp_nice e2
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
