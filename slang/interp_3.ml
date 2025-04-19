(**************************************
Compiler Construction 2020
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)

(** Interpreter 3.

    Derived from Interpreter 2 by

    --- Make instructions linear by introducing labels and jumps.

    --- labels translated to numeric addresses.

    --- include "code pointer" in state

    --- compiler elimnates WHILE construct *)

type address = int
type label = string
type location = label * address option

type value = f Value.t
and f = Closure of location * env | Rec_closure of location

and instruction =
  | PUSH of value
  | LOOKUP of Ast.var
  | UNARY of Ast.Unary_op.t
  | OPER of Ast.Binary_op.t
  | ASSIGN
  | SWAP
  | POP
  | BIND of Ast.var
  | FST
  | SND
  | DEREF
  | APPLY
  | RETURN
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of location
  | MK_REC of Ast.var * location
  | TEST of location
  | CASE of location
  | GOTO of location
  | LABEL of label
  | HALT

and code = instruction list
and binding = Ast.var * value
and env = binding list

type env_or_value =
  | EV of env (* an environment on the run-time stack *)
  | V of value (* a value on the run-time stack *)
  | RA of address (* a return address on the run-time stack *)

type env_value_stack = env_or_value list
type state = address * env_value_stack

(* update : (env * binding) -> env *)
(* let update(env, (x, v)) = (x, v) :: env *)

let rec lookup ((env, x) : env * Ast.var) : value option =
  match env with
  | [] -> None
  | (y, v) :: rest ->
      if x = y then
        Some
          (match v with
          | `Fun (Rec_closure loc) ->
              `Fun (Closure (loc, (y, `Fun (Rec_closure loc)) :: rest))
          | _ -> v)
      else
        lookup (rest, x)

let rec search (evs, x) =
  match evs with
  | [] -> Errors.complainf "%s is not defined!@\n" x
  | V _ :: rest -> search (rest, x)
  | RA _ :: rest -> search (rest, x)
  | EV env :: rest -> (
      match lookup (env, x) with None -> search (rest, x) | Some v -> v)

let rec evs_to_env = function
  | [] -> []
  | V _ :: rest -> evs_to_env rest
  | RA _ :: rest -> evs_to_env rest
  | EV env :: rest -> env @ evs_to_env rest

let pp_list fmt sep f l =
  let rec aux f fmt = function
    | [] -> ()
    | [ t ] -> f fmt t
    | t :: rest -> Format.fprintf fmt "%a%(%)%a" f t sep (aux f) rest
  in
  Format.fprintf fmt "@[[%a]@]" (aux f) l

let rec pp_value fmt : value -> unit = Value.pp pp_fun fmt

and pp_fun fmt = function
  | Closure (loc, env) ->
      Format.fprintf fmt "(%a, %a)" pp_location loc pp_env env
  | Rec_closure _ -> Format.fprintf fmt ""

and pp_env fmt env = pp_list fmt ",@\n " pp_binding env
and pp_binding fmt (x, v) = Format.fprintf fmt "(%s, %a)" x pp_value v

and pp_location fmt = function
  | l, None -> Format.fprintf fmt "%s" l
  | l, Some i -> Format.fprintf fmt "%s = %d" l i

and pp_instruction fmt = function
  | UNARY op -> Format.fprintf fmt "  UNARY %s" (Ast.Unary_op.to_string op)
  | OPER op -> Format.fprintf fmt "  OPER %s" (Ast.Binary_op.to_string op)
  | MK_PAIR -> Format.fprintf fmt "  MK_PAIR"
  | FST -> Format.fprintf fmt "  FST"
  | SND -> Format.fprintf fmt "  SND"
  | MK_INL -> Format.fprintf fmt "  MK_INL"
  | MK_INR -> Format.fprintf fmt "  MK_INR"
  | MK_REF -> Format.fprintf fmt "  MK_REF"
  | PUSH v -> Format.fprintf fmt "  PUSH %a" pp_value v
  | LOOKUP x -> Format.fprintf fmt "  LOOKUP %s" x
  | TEST label -> Format.fprintf fmt "  TEST %a" pp_location label
  | CASE label -> Format.fprintf fmt "  CASE %a" pp_location label
  | GOTO label -> Format.fprintf fmt "  GOTO %a" pp_location label
  | APPLY -> Format.fprintf fmt "  APPLY"
  | RETURN -> Format.fprintf fmt "  RETURN"
  | HALT -> Format.fprintf fmt "  HALT"
  | BIND x -> Format.fprintf fmt "  BIND %s" x
  | LABEL label -> Format.fprintf fmt "LABEL %s:" label
  | SWAP -> Format.fprintf fmt "  SWAP"
  | POP -> Format.fprintf fmt "  POP"
  | DEREF -> Format.fprintf fmt "  DEREF"
  | ASSIGN -> Format.fprintf fmt "  ASSIGN"
  | MK_CLOSURE loc -> Format.fprintf fmt "  MK_CLOSURE(%a)" pp_location loc
  | MK_REC (v, loc) ->
      Format.fprintf fmt "  MK_REC(@[%s, %a)@]" v pp_location loc

and pp_code fmt c = List.iter (Format.fprintf fmt "%a@\n" pp_instruction) c

let pp_env_or_value fmt = function
  | EV env -> Format.fprintf fmt "EV %a" pp_env env
  | V v -> Format.fprintf fmt "V %a" pp_value v
  | RA i -> Format.fprintf fmt "RA %d" i

let pp_env_value_stack fmt = pp_list fmt ";@\n " pp_env_or_value
let string_of_value = Format.asprintf "%a" pp_value
let string_of_location = Format.asprintf "%a" pp_location
let string_of_code = Format.asprintf "%a" pp_code
let string_of_env_or_value = Format.asprintf "%a" pp_env_or_value

(* THE MACHINE *)

let installed = ref (Array.of_list [ HALT ])

let pp_installed_code fmt =
  let size = Array.length !installed in
  let rec aux fmt k =
    if size <> k then
      Format.fprintf fmt "%d: %a@\n%a" k pp_instruction !installed.(k) aux
        (k + 1)
  in
  aux fmt 0

let string_of_installed_code () =
  Format.asprintf "%a" (fun f () -> pp_installed_code f) ()

let get_instruction cp = Array.get !installed cp
let heap = Array.make Option.heap_max (`Int 0)
let next_address = ref 0

let new_address () =
  let a = !next_address in
  next_address := a + 1;
  a

let string_of_heap () =
  let rec aux k =
    if !next_address < k then
      ""
    else
      string_of_int k ^ " -> " ^ string_of_value heap.(k) ^ "\n" ^ aux (k + 1)
  in
  "\nHeap = \n" ^ aux 0

let pp_state fmt (cp, evs) =
  Format.fprintf fmt "@\nCode Pointer = %d -> %a@\nStack = %a%s@\n" cp
    pp_instruction (get_instruction cp) pp_env_value_stack evs
    (if !next_address = 0 then
       ""
     else
       string_of_heap ())

let step (cp, evs) =
  match (get_instruction cp, evs) with
  | PUSH v, evs -> (cp + 1, V v :: evs)
  | POP, _ :: evs -> (cp + 1, evs)
  | SWAP, s1 :: s2 :: evs -> (cp + 1, s2 :: s1 :: evs)
  | BIND x, V v :: evs -> (cp + 1, EV [ (x, v) ] :: evs)
  | LOOKUP x, evs -> (cp + 1, V (search (evs, x)) :: evs)
  | UNARY op, V v :: evs -> (cp + 1, V (Ast.Unary_op.to_fun op v) :: evs)
  | OPER op, V v2 :: V v1 :: evs ->
      (cp + 1, V (Ast.Binary_op.to_fun op (v1, v2)) :: evs)
  | MK_PAIR, V v2 :: V v1 :: evs -> (cp + 1, V (`Pair (v1, v2)) :: evs)
  | FST, V (`Pair (v, _)) :: evs -> (cp + 1, V v :: evs)
  | SND, V (`Pair (_, v)) :: evs -> (cp + 1, V v :: evs)
  | MK_INL, V v :: evs -> (cp + 1, V (`Inl v) :: evs)
  | MK_INR, V v :: evs -> (cp + 1, V (`Inr v) :: evs)
  | CASE (_, Some _), V (`Inl v) :: evs -> (cp + 1, V v :: evs)
  | CASE (_, Some i), V (`Inr v) :: evs -> (i, V v :: evs)
  | TEST (_, Some _), V (`Bool true) :: evs -> (cp + 1, evs)
  | TEST (_, Some i), V (`Bool false) :: evs -> (i, evs)
  | ASSIGN, V v :: V (`Ref a) :: evs ->
      heap.(a) <- v;
      (cp + 1, V `Unit :: evs)
  | DEREF, V (`Ref a) :: evs -> (cp + 1, V heap.(a) :: evs)
  | MK_REF, V v :: evs ->
      let a = new_address () in
      heap.(a) <- v;
      (cp + 1, V (`Ref a) :: evs)
  | MK_CLOSURE loc, evs ->
      (cp + 1, V (`Fun (Closure (loc, evs_to_env evs))) :: evs)
  | MK_REC (f, loc), evs ->
      ( cp + 1,
        V (`Fun (Closure (loc, (f, `Fun (Rec_closure loc)) :: evs_to_env evs)))
        :: evs )
  | APPLY, V (`Fun (Closure ((_, Some i), env))) :: V v :: evs ->
      (i, V v :: EV env :: RA (cp + 1) :: evs)
  (* new intructions *)
  | RETURN, V v :: _ :: RA i :: evs -> (i, V v :: evs)
  | LABEL _, evs -> (cp + 1, evs)
  | HALT, evs -> (cp, evs)
  | GOTO (_, Some i), evs -> (i, evs)
  | _ -> Errors.complainf "step : bad state = %a\n" pp_state (cp, evs)

(* COMPILE *)

let label_ref = ref 0

let new_label =
  let get () =
    let v = !label_ref in
    label_ref := !label_ref + 1;
    "L" ^ string_of_int v
  in
  get

let rec comp : Ast.t -> code * code = function
  | Unit -> ([], [ PUSH `Unit ])
  | Integer n -> ([], [ PUSH (`Int n) ])
  | Boolean b -> ([], [ PUSH (`Bool b) ])
  | Var x -> ([], [ LOOKUP x ])
  | UnaryOp (op, e) ->
      let defs, c = comp e in
      (defs, c @ [ UNARY op ])
  | BinaryOp (e1, op, e2) ->
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      (defs1 @ defs2, c1 @ c2 @ [ OPER op ])
  | Pair (e1, e2) ->
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      (defs1 @ defs2, c1 @ c2 @ [ MK_PAIR ])
  | Fst e ->
      let defs, c = comp e in
      (defs, c @ [ FST ])
  | Snd e ->
      let defs, c = comp e in
      (defs, c @ [ SND ])
  | Inl e ->
      let defs, c = comp e in
      (defs, c @ [ MK_INL ])
  | Inr e ->
      let defs, c = comp e in
      (defs, c @ [ MK_INR ])
  | Case (e1, (x1, e2), (x2, e3)) ->
      let inr_label = new_label () in
      let after_inr_label = new_label () in
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      let defs3, c3 = comp e3 in
      ( defs1 @ defs2 @ defs3,
        c1
        @ [ CASE (inr_label, None) ]
        @ ((BIND x1 :: c2) @ [ SWAP; POP ])
        @ [ GOTO (after_inr_label, None); LABEL inr_label ]
        @ ((BIND x2 :: c3) @ [ SWAP; POP ])
        @ [ LABEL after_inr_label ] )
  | If (e1, e2, e3) ->
      let else_label = new_label () in
      let after_else_label = new_label () in
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      let defs3, c3 = comp e3 in
      ( defs1 @ defs2 @ defs3,
        c1
        @ [ TEST (else_label, None) ]
        @ c2
        @ [ GOTO (after_else_label, None); LABEL else_label ]
        @ c3 @ [ LABEL after_else_label ] )
  | Seq [] -> ([], [])
  | Seq [ e ] -> comp e
  | Seq (e :: rest) ->
      let defs1, c1 = comp e in
      let defs2, c2 = comp (Seq rest) in
      (defs1 @ defs2, c1 @ [ POP ] @ c2)
  | Ref e ->
      let defs, c = comp e in
      (defs, c @ [ MK_REF ])
  | Deref e ->
      let defs, c = comp e in
      (defs, c @ [ DEREF ])
  | While (e1, e2) ->
      let test_label = new_label () in
      let end_label = new_label () in
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      ( defs1 @ defs2,
        [ LABEL test_label ] @ c1
        @ [ TEST (end_label, None) ]
        @ c2
        @ [ POP; GOTO (test_label, None); LABEL end_label; PUSH `Unit ] )
  | Assign (e1, e2) ->
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      (defs1 @ defs2, c1 @ c2 @ [ ASSIGN ])
  | App (e1, e2) ->
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      (defs1 @ defs2, c2 @ c1 @ [ APPLY ])
  | Lambda (x, e) ->
      let defs, c = comp e in
      let f = new_label () in
      let def = [ LABEL f; BIND x ] @ c @ [ SWAP; POP; RETURN ] in
      (def @ defs, [ MK_CLOSURE (f, None) ])
  (*
 Note that we could have

 | LetFun(f, (x, e1), e2) -> comp (App(Lambda(f, e2), Lambda(x, e1)))

 This would then result (ignoring the defs generated by subterms) in

    defs = [LABEL g ; BIND f] @ c2 @ [SWAP; POP; RETURN]
         @ [LABEL h; bind x ] @ c1 @ [SWAP; POP; RETURN]

    code = [MK_CLOSURE((h, None)); [MK_CLOSURE((g, None)); APPLY]

  where g and h are new labels.

  In contrast, the following version of comp results in

     defs = [LABEL f; BIND x] @ c1 @ [SWAP; POP; RETURN]

     code = [MK_CLOSURE((f, None)); BIND f] @ c2 @ [SWAP; POP])

  which is simpler.

*)
  | LetFun (f, (x, e1), e2) ->
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      let lab = new_label () in
      let def = [ LABEL lab; BIND x ] @ c1 @ [ SWAP; POP; RETURN ] in
      ( def @ defs1 @ defs2,
        [ MK_CLOSURE (lab, None); BIND f ] @ c2 @ [ SWAP; POP ] )
  | LetRecFun (f, (x, e1), e2) ->
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      let lab = new_label () in
      let def = [ LABEL lab; BIND x ] @ c1 @ [ SWAP; POP; RETURN ] in
      ( def @ defs1 @ defs2,
        [ MK_REC (f, (lab, None)); BIND f ] @ c2 @ [ SWAP; POP ] )

let compile e =
  let defs, c = comp e in
  (* The HALT instruction needs an annotation to satisfy the types
     We arbitarily use the annotation from the root of the AST
   *)
  let result =
    c (* body of program *) @ [ HALT ] (* stop the interpreter *) @ defs
  in
  (* the function definitions *)
  let () =
    if Option.verbose then
      Format.printf "@\nCompiled Code = @\n%a" pp_code result
  in
  result

let rec driver n ((cp, evs) as state) =
  if Option.verbose then
    Format.printf "\nstate %d:%a\n" n pp_state state;
  match (get_instruction cp, evs) with
  | HALT, [ V v ] -> v
  | HALT, _ ->
      Errors.complainf "driver : bad halted state = %a\n" pp_state state
  | _ -> driver (n + 1) (step state)

(* put code listing into an array, associate an array index to each label *)
let load l =
  let rec find lab = function
    | [] -> Errors.complainf "find : %s is not found" lab
    | (x, v) :: rest ->
        if x = lab then
          v
        else
          find lab rest
    (* insert array index for each label *)
  in
  let apply_label_map_to_instruction m = function
    | GOTO (lab, _) -> GOTO (lab, Some (find lab m))
    | TEST (lab, _) -> TEST (lab, Some (find lab m))
    | CASE (lab, _) -> CASE (lab, Some (find lab m))
    | MK_CLOSURE (lab, _) -> MK_CLOSURE (lab, Some (find lab m))
    | MK_REC (f, (lab, _)) -> MK_REC (f, (lab, Some (find lab m)))
    | inst -> inst
  in
  (* find array index for each label *)
  let listing_to_label_map l =
    let rec aux carry k = function
      | [] -> carry
      | LABEL lab :: rest -> aux ((lab, k) :: carry) (k + 1) rest
      | _ :: rest -> aux carry (k + 1) rest
    in
    aux [] 0 l
  in
  let l_map = listing_to_label_map l in
  Array.of_list (List.map (apply_label_map_to_instruction l_map) l)

let interpret (e : Ast.t) : value =
  let c = compile e in
  installed := load c;
  if Option.verbose then
    Format.printf "\nInstalled Code = \n%s" (string_of_installed_code ());
  (* set the code pointer to 0 *)
  driver 1 (0, [])

let reset () =
  next_address := 0;
  label_ref := 0;
  Array.fill heap 0 (Array.length heap) (`Int 0)
