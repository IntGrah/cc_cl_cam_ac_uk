open Slanglib

let string_state (cp, evs, heap_list) =
  ( cp,
    List.map (Format.asprintf "%a" Interp_3.pp_env_or_value) evs,
    List.map (Format.asprintf "%a" Interp_3.pp_value) heap_list )

let list_of_heap _ =
  Array.to_list (Array.sub Interp_3.heap 0 !Interp_3.next_address)

let rec driver n (cp, env) =
  let heapl = list_of_heap () in
  (cp, env, heapl)
  ::
  (if Interp_3.HALT = Interp_3.get_instruction cp then
     []
   else
     driver (n + 1) (Interp_3.step (cp, env)))

let stacks e =
  let c = Interp_3.compile e in
  let _ = Interp_3.installed := Interp_3.load c in
  let installed_code =
    Format.asprintf "%a" (fun fmt () -> Interp_3.pp_installed_code fmt) ()
  in
  (installed_code, List.map string_state (driver 1 (0, [])))

let string_list_of_code c =
  List.flatten
  @@ List.map
       (fun instr ->
         let buf = Buffer.create 16 in
         let ppf = Format.formatter_of_buffer buf in
         Interp_3.pp_instruction ppf instr;
         Format.pp_print_flush ppf ();
         [ Buffer.contents buf ])
       c
