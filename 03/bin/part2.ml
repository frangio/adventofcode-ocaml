let read_cable () =
  let line = read_line () in
  let turns = String.split_on_char ',' line in
  List.map Day03.Wires.turn_of_string turns

exception Invalid_inputs

let () =
  let cable1 = read_cable () in
  let cable2 = read_cable () in
  let cost = Day03.Wires.fastest_intersection [cable1; cable2] in
  print_int cost
