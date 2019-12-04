type computer = int list * int

type opcode = Add | Mul | Halt

exception Invalid_opcode

let read_args (program, position) =
  let read_offset i = List.nth program (position + i) in
  let deref_offset i = List.nth program (read_offset i) in
  (deref_offset 1, deref_offset 2, read_offset 3)

let%test _ = read_args ([1; 1; 2; 3], 0) = (1, 2, 3)
let%test _ = read_args ([1; 2; 1; 3], 0) = (1, 2, 3)
let%test _ = read_args ([1; 9; 10; 3; 2; 3; 11; 0; 99; 30; 40; 50], 0) = (30, 40, 3)

let read_opcode (program, position) =
  match List.nth program position with
  | 1 -> Add
  | 2 -> Mul
  | 99 -> Halt
  | _ -> raise Invalid_opcode

let store_at index value list =
  List.mapi (fun i x -> if i = index then value else x) list

let step_binary fn ((program, position) as computer) =
  let (src1, src2, dest) = read_args computer in
  (store_at dest (fn src1 src2) program, position + 4)

let step computer =
  let opcode = read_opcode computer in
  match opcode with
  | Halt -> computer
  | Add -> step_binary ( + ) computer
  | Mul -> step_binary ( * ) computer

let%test _ = step ([1; 1; 1; 3], 0) = ([1; 1; 1; 2], 4)
let%test _ = step ([1; 4; 5; 3; 5; 6], 0) = ([1; 4; 5; 11; 5; 6], 4)
let%test _ = step ([1; 9; 10; 3; 2; 3; 11; 0; 99; 30; 40; 50], 0)
  = ([1; 9; 10; 70; 2; 3; 11; 0; 99; 30; 40; 50], 4)

let is_halted computer = read_opcode computer = Halt

let rec run computer =
  let next = step computer in
  if is_halted next then next else run next

let%test _ = run ([1; 1; 1; 3; 99], 0) = ([1; 1; 1; 2; 99], 4)

let run_1202 program =
  let altered_program = program |> (store_at 1 12) |> (store_at 2 2) in
  let (result, _) = run (altered_program, 0) in
  List.nth result 0

