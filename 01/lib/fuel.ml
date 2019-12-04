let of_module mass = 
  let rec calc mass =
    let fuel = max 0 (mass / 3 - 2) in
    if fuel = 0 then 0 else fuel + calc fuel
  in calc mass

let%test _ = of_module 12 = 2

let of_modules masses = List.fold_left (+) 0 (List.map of_module masses)

(* let () = assert (2 = (Fuel.module_count 12)) *)
(* let () = assert (2 = (Fuel.module_count 14)) *)
(* let () = assert (654 = (Fuel.module_count 1969)) *)
(* let () = assert (33583 = (Fuel.module_count 100756)) *)
(* let () = assert (4 = (Fuel.total_count [12; 14])) *)
(* let () = assert (658 = (Fuel.total_count [12; 14; 1969])) *)
