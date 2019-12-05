let read_program () =
  let line = read_line () in
  let codes = String.split_on_char ',' line in
  List.map int_of_string codes

exception Invalid_inputs

let read_inputs () =
  let line = read_line () in
  let codes = String.split_on_char ',' line in
  match List.map int_of_string codes with
  | [noun; verb] -> (noun, verb)
  | _ -> raise Invalid_inputs

let () =
  let program = read_program () in
  for noun = 0 to List.length program do
    for verb = 0 to List.length program do
      try
        let output = Day02.Intcode.run_with program noun verb in
        if output = 19690720 then
          print_endline @@ string_of_int (100 * noun + verb)
        else
          ()
      with _ -> ()
    done
  done
