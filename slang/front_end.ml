let error file action s =
  Errors.complain ("\nERROR in " ^ file ^ " with " ^ action ^ " : " ^ s ^ "\n")

let print_if_verbose m e pp =
  if Option.verbose_front then
    print_endline
      (m ^ ":\n"
      ^
      if Option.verbose_tree then
        Pptree.pp_no_bracket (pp e)
      else
        pp e)

let parse_error file (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let line = string_of_int pos.pos_lnum in
  let pos = string_of_int (pos.pos_cnum - pos.pos_bol + 1) in
  error file "parsing" ("at line " ^ line ^ " position " ^ pos)

(* Initialize lexer *)
let init_lexbuf file =
  let in_chan =
    try open_in file
    with _ -> error file "initialize lexer" ("can't open file " ^ file)
  in
  let lexbuf = Lexing.from_channel in_chan in

  lexbuf.lex_curr_p <-
    { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };

  (file, lexbuf)

(* Parse input file *)
let parse (file, lexbuf) =
  let e =
    try Parser.start Lexer.token lexbuf
    with Parsing.Parse_error -> parse_error file lexbuf
  in
  print_if_verbose "Parsed result" e Past.to_string;
  (file, e)

(* Perform static checks and translate from Past to Ast *)
let check (file, e) =
  let e', _ =
    try Static.elab [] e with
    | Errors.Error s -> error file "static check" s
    | Static.Type_error _ -> error file "type check" "type error"
  in
  print_if_verbose "After static checks" e' Ast.to_string;
  e'

(* The front end *)
let front_end file = check (parse (init_lexbuf file))

let front_end_from_string str =
  let initstrbuf str =
    let lexbuf = Lexing.from_string str in
    lexbuf.lex_curr_p <-
      { pos_fname = "input"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
    ("input", lexbuf)
  in
  check (parse (initstrbuf str))
