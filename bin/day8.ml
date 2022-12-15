(****
    Day 8
      *****)
open Core
open Aoc2022_lib

let sight_lines g x y =
  let h = IntGrid.height g in
  let w = IntGrid.width g in
  let get_on_col x y = IntGrid.get_exn g ~col:x ~row:y in
  let get_on_row y x = IntGrid.get_exn g ~col:x ~row:y in
  [
    (* left *)
    List.map (List.rev (List.range 0 x)) ~f:(get_on_row y);
    (* top *)
    List.map (List.rev (List.range 0 y)) ~f:(get_on_col x);
    (* right *)
    List.map (List.range (x + 1) w) ~f:(get_on_row y);
    (* bottom *)
    List.map (List.range (y + 1) h) ~f:(get_on_col x);
  ]

let is_visible_global g x y =
  let v = IntGrid.get_exn g ~col:x ~row:y in
  let is_vis_slice = List.for_all ~f:(fun x -> x < v) in
  sight_lines g x y |> List.map ~f:is_vis_slice |> or_bools

let count_visible g =
  IntGrid.positions g |> List.count ~f:(fun (x, y) -> is_visible_global g x y)

let scenic_score g x y =
  let v = IntGrid.get_exn g ~col:x ~row:y in
  let num_vis_in_line sl =
    let smaller = List.take_while sl ~f:(fun x -> x < v) in
    let boundary = List.drop_while sl ~f:(fun x -> x < v) in
    let num_smaller = List.length smaller in
    if List.is_empty boundary then num_smaller else num_smaller + 1
  in
  sight_lines g x y |> List.map ~f:num_vis_in_line |> mult_ints

let best_scenic_score g =
  IntGrid.positions g
  |> List.map ~f:(fun (x, y) -> scenic_score g x y)
  |> List.max_elt ~compare:Int.compare
  |> Option.value ~default:0

let solve params lines =
  let g = IntGrid.from_lines ~f:(fun c -> int_of_string (Char.to_string c)) lines in
  match params.(1) |> int_of_string with
  | 1 -> count_visible g
  | 2 -> best_scenic_score g
  | _ -> raise @@ Failure "invalid part"
