type point = int * int
type direction = Right | Left | Up | Down
type turn = direction * int
type cable = turn list

exception Unknown_direction

let turn_of_string str =
  let dir =
    match String.get str 0 with
    | 'R' -> Right
    | 'L' -> Left
    | 'U' -> Up
    | 'D' -> Down
    | _ -> raise Unknown_direction
  in
  let length = int_of_string (String.sub str 1 (String.length str - 1)) in
  (dir, length)

let%test _ = turn_of_string "R1" = (Right, 1)
let%test _ = turn_of_string "U10" = (Up, 10)

let step dir (x, y) : point =
  match dir with
  | Right -> (x + 1, y)
  | Left -> (x - 1, y)
  | Up -> (x, y + 1)
  | Down -> (x, y - 1)

let list_points cable : point list =
  let rec list_points' cable origin =
    match cable with
    | (dir, length) :: rest ->
        let new_position = step dir origin in
        let remaining_cable =
          match length with
          | 1 -> rest
          | _ -> (dir, length - 1) :: rest in
        new_position :: list_points' remaining_cable new_position
    | [] -> []
  in
  list_points' cable (0, 0)

let%test _ = list_points [(Right, 1)] = [(1, 0)]
let%test _ = list_points [(Right, 2)] = [(1, 0); (2, 0)]
let%test _ = list_points [(Right, 1); (Left, 1)] = [(1, 0); (0, 0)]
let%test _ = list_points [(Right, 1); (Up, 1)] = [(1, 0); (1, 1)]

let manhattan_distance (x1, y1) (x2, y2) =
  (abs (x1 - x2)) + (abs (y1 - y2))

module Point =
  struct
    type t = point
    let compare p1 p2 =
      let manhattan = Stdlib.compare (manhattan_distance p1 (0, 0)) (manhattan_distance p2 (0, 0)) in
      if manhattan = 0 then Stdlib.compare p1 p2 else manhattan
  end

module PointMap = Map.Make(Point)

let intersections cables : point Seq.t =
  let cable_counts =
    cables
    |> List.map (fun cable ->
        cable
        |> list_points
        |> List.map (fun p -> (p, 1))
        |> List.to_seq
        |> PointMap.of_seq)
    |> List.fold_left (PointMap.union (fun _ a b -> Some (a + b))) PointMap.empty
  in
  cable_counts
  |> PointMap.to_seq
  |> Seq.filter (fun (_, count) -> count >= 2)
  |> Seq.map (fun (p, _) -> p) 

exception No_intersections

let closest_intersection cables : point =
  let iseq = intersections cables in
  match iseq () with
  | Cons (first, _) -> first
  | Nil -> raise No_intersections

let%test _ = closest_intersection [[(Right, 1)]; [(Right, 1)]] = (1, 0)
let%test _ = closest_intersection [[(Right, 1); (Up, 1)]; [(Up, 1); (Right, 1)]] = (1, 1)

exception Unreachable_point

let cost_to_reach point cable : int =
  let cost = ref None in
  let () = List.iteri (fun i p -> if point = p then cost := Some i) (list_points cable) in
  match !cost with
  | Some k -> k + 1
  | None -> raise Unreachable_point

let%test _ = cost_to_reach (1, 0) [(Right, 5)] = 1
let%test _ = cost_to_reach (4, 0) [(Right, 5)] = 4
let%test _ = cost_to_reach (5, 2) [(Right, 5); (Up, 5)] = 7

let fastest_intersection cables : int =
  let points = List.of_seq (intersections cables) in
  let costs =
    List.map (fun p -> (p,
      cables
      |> List.map (cost_to_reach p)
      |> List.fold_left (+) 0))
    points
  in
  let (_, k) =
    List.fold_left (fun (p1, k1) (p2, k2) -> if k1 < k2 then (p1, k1) else (p2, k2))
    (List.hd costs)
    (List.tl costs)
  in
  k

let%test _ = fastest_intersection [[(Right, 1)]; [(Right, 1)]] = 2
