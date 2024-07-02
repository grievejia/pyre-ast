let () =
  let input_content = Stdio.In_channel.read_all Sys.argv.(1) in
  let output_content =
    Str.global_replace (Str.regexp {|\$(\([A-Za-z0-9]*\))|}) "${\\1}" input_content
  in
  Stdio.Out_channel.write_all "python-config-fixed.sh" ~data:output_content
