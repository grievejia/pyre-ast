open Sexplib

let extract_flags script =
  let in_channel = Unix.open_process_args_in "/bin/sh" [| "/bin/sh"; script; "--libs" |] in
  let line = input_line in_channel |> String.trim in
  let _ = Unix.close_process_in in_channel in
  String.split_on_char ' ' line |> List.filter (fun word -> String.length word > 0)

let () =
  let flags = extract_flags Sys.argv.(1) in
  let sexp = Sexp.List (List.map (fun f -> Sexp.Atom f) flags) in
  Sexp.save_mach "ldflags.sexp" sexp
