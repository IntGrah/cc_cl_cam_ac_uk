let test_dir = "tests/"
let manifest_file = test_dir ^ "manifest.txt"
let process_line file expected = Some (test_dir ^ file ^ ".slang", Some expected)

let scan_line in_chan =
  try Scanf.bscanf in_chan "%s %s\n" process_line with
  | End_of_file -> None
  | ex ->
      Format.eprintf "Unexpected Scanf exception : %s" (Printexc.to_string ex);
      None

let rec get_all in_chan =
  match scan_line in_chan with
  | None -> []
  | Some (f, e) -> (f, e) :: get_all in_chan

let get_all_tests () =
  let in_chan =
    try Scanf.Scanning.open_in manifest_file
    with _ -> Errors.complainf "can't open file %s" manifest_file
  in
  get_all in_chan
