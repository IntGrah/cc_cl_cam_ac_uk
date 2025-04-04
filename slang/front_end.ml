open Lexing

let error file action s =
  Errors.complain ("\nERROR in " ^ file ^ " with " ^ action ^ " : " ^ s ^ "\n")

let print_if_verbose m e pp =
  if Option.verbose_front then
    print_string
      (m ^ ":\n"
      ^ (if Option.verbose_tree then Pptree.pp_no_bracket else fun x -> x)
          (pp e)
      ^ "\n")

let parse_error file lexbuf =
  let pos = lexbuf.lex_curr_p in
  let line = string_of_int pos.pos_lnum in
  let pos = string_of_int (pos.pos_cnum - pos.pos_bol + 1) in
  error file "parsing" ("at line " ^ line ^ " position " ^ pos)

(* initialize lexer *)
let init_lexbuf file =
  let in_chan =
    try open_in file
    with _ -> error file "initialize lexer" ("can't open file " ^ file)
  in
  let lexbuf = from_channel in_chan in
  let _ =
    lexbuf.lex_curr_p <-
      { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  (file, lexbuf)

(* parse input file *)
let parse (file, lexbuf) =
  let e =
    try Parser.start Lexer.token lexbuf
    with Parsing.Parse_error -> parse_error file lexbuf
  in
  let _ = print_if_verbose "Parsed result" e Past.to_string in
  (file, e)

(* perform static checks *)
let check (file, e) =
  let e', _ =
    try Static.elab [] e with
    | Errors.Error s -> error file "static check" s
    | Static.Type_error _ -> error file "type check" "type error"
  in
  let _ = print_if_verbose "After static checks" e' Ast.to_string in
  e'

(* translate from Past.expr to Ast.expr *)

(* the front end *)
let front_end file = check (parse (init_lexbuf file))

(* front end reading directly from string *)
let initstrbuf str =
  let lexbuf = from_string str in
  let _ =
    lexbuf.lex_curr_p <-
      { pos_fname = "input"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  ("input", lexbuf)

let front_end_from_string str = check (parse (initstrbuf str))
