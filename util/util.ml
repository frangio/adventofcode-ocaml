let read_lines file =
  let lines = ref [] in
  let chan = open_in file in
  let () = try
    while true do
      lines := input_line chan :: !lines
    done
  with End_of_file ->
    close_in chan
  in !lines

