let read_program () =
  let line = read_line () in
  let codes = String.split_on_char ',' line in
  List.map int_of_string codes

let () =
  let program = read_program () in
  let result = Day02.Intcode.run_1202 program in
  print_int result
