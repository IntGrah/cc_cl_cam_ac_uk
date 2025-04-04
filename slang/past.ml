type var = string

module Loc = struct
  type t = Lexing.position

  let pp ppf (loc : t) =
    Format.fprintf ppf "line %d, position %d" loc.pos_lnum
      (loc.pos_cnum - loc.pos_bol + 1)
end

type unary_op = [ `Neg | `Not ]
type binary_op = [ `Add | `Sub | `Mul | `Div | `Lt | `And | `Or | `Eq ]

type t = { loc : Loc.t; expr : expr }

and expr =
  | Unit
  | What
  | Var of var
  | Integer of int
  | Boolean of bool
  | UnaryOp of unary_op * t
  | BinaryOp of t * binary_op * t
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

let rec to_string t =
  match t.expr with
  | Unit -> "Unit"
  | What -> "?"
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
  | Inl (t, e) -> Format.sprintf "Inl(%s, %s)" (Type.to_string t) (to_string e)
  | Inr (t, e) -> Format.sprintf "Inr(%s, %s)" (Type.to_string t) (to_string e)
  | Case (e, (x1, t1, e1), (x2, t2, e2)) ->
      Format.sprintf "Case(%s, (%s, %s, %s), (%s, %s, %s))" (to_string e) x1
        (Type.to_string t1) (to_string e1) x2 (Type.to_string t2) (to_string e2)
  | While (e1, e2) ->
      Format.sprintf "While(%s, %s)" (to_string e1) (to_string e2)
  | Seq el ->
      Format.sprintf "Seq(%s)"
        (List.map (fun e -> to_string e) el |> String.concat "; ")
  | Ref e -> Format.sprintf "Ref(%s)" (to_string e)
  | Deref e -> Format.sprintf "Deref(%s)" (to_string e)
  | Assign (e1, e2) ->
      Format.sprintf "Assign(%s, %s)" (to_string e1) (to_string e2)
  | Lambda (x, t, e) ->
      Format.sprintf "Lambda(%s, %s, %s)" x (Type.to_string t) (to_string e)
  | App (e1, e2) -> Format.sprintf "App(%s, %s)" (to_string e1) (to_string e2)
  | Let (x, t, e1, e2) ->
      Format.sprintf "Let(%s, %s, %s, %s)" x (Type.to_string t) (to_string e1)
        (to_string e2)
  | LetFun (f, (x, t1, e1), t2, e2) ->
      Format.sprintf "LetFun(%s, (%s, %s, %s), %s, %s)" f x (Type.to_string t1)
        (to_string e1) (Type.to_string t2) (to_string e2)

(*
   Documentation of Format can be found here:
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*)

let rec pp ppf t =
  let open Format in
  match t.expr with
  | Unit -> fprintf ppf "()"
  | What -> fprintf ppf "?"
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
  | Inl (t, e) -> fprintf ppf "(inl %a %a)" Type.pp t pp e
  | Inr (t, e) -> fprintf ppf "(inr %a %a)" Type.pp t pp e
  | Case (e, (x1, t1, e1), (x2, t2, e2)) ->
      fprintf ppf
        "@[<2>case %a of@ | inl(%s : %a) -> %a @ | inr(%s : %a) -> %a end@]" pp
        e x1 Type.pp t1 pp e1 x2 Type.pp t2 pp e2
  | Seq [] -> ()
  | Seq [ e ] -> pp ppf e
  | Seq (e :: rest) ->
      fprintf ppf "%a; %a" pp e pp { loc = t.loc; expr = Seq rest }
  | While (e1, e2) -> fprintf ppf "while %a do %a" pp e1 pp e2
  | Ref e -> fprintf ppf "ref %a" pp e
  | Deref e -> fprintf ppf "!%a" pp e
  | Assign (e1, e2) -> fprintf ppf "(%a := %a)" pp e1 pp e2
  | Lambda (x, t, e) -> fprintf ppf "(fun %s : %a -> %a)" x Type.pp t pp e
  | App (e1, e2) -> fprintf ppf "%a %a" pp e1 pp e2
  | Let (x, t, e1, e2) ->
      fprintf ppf "@[<2>let %s : %a = %a in %a end@]" x Type.pp t pp e1 pp e2
  | LetFun (f, (x, t1, e1), t2, e2) ->
      fprintf ppf "@[let %s (%s : %a) : %a =@ %a @ in %a @ end@]" f x Type.pp t1
        Type.pp t2 pp e1 pp e2
