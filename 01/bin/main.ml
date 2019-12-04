let read_masses file = List.map int_of_string (Util.read_lines file)

let () = print_int (Day01.Fuel.of_modules (read_masses Sys.argv.(1)))
