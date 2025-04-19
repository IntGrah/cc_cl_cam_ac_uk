let error filename action s =
  Errors.complainf "ERROR in %s with %s : %s@." filename action s

let print_if_verbose m pp e =
  if Option.verbose_front then (
    print_endline m;
    print_endline
      (if Option.verbose_tree then
         Pptree.pp_no_bracket (pp e)
       else
         pp e))

let parse_error filename (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let line = string_of_int pos.pos_lnum in
  let pos = string_of_int (pos.pos_cnum - pos.pos_bol + 1) in
  error filename "parsing" ("at line " ^ line ^ " position " ^ pos)

(* Parse input file *)
let parse file lexbuf =
  let e =
    try Parser.start Lexer.token lexbuf
    with Parsing.Parse_error -> parse_error file lexbuf
  in
  print_if_verbose "Parsed result:" Past.to_string e;
  e

(* Perform static checks and translate from Past to Ast *)
let check filename e =
  let e' =
    try Static.translate e
    with Type.Type_error _ -> error filename "type check" "type error"
  in
  print_if_verbose "After static checks:" Ast.to_string e';
  e'

(* The front end *)
let front_end filename =
  let init_lexbuf file =
    let in_chan =
      try open_in file
      with _ -> error file "initialize lexer" ("can't open file " ^ file)
    in
    let lexbuf = Lexing.from_channel in_chan in

    lexbuf.lex_curr_p <-
      { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };

    lexbuf
  in
  init_lexbuf filename |> parse filename |> check filename

let front_end_from_string str =
  let filename = "input" in
  let init_lexbuf_str str =
    let lexbuf = Lexing.from_string str in
    lexbuf.lex_curr_p <-
      { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
    lexbuf
  in
  init_lexbuf_str str |> parse filename |> check filename
