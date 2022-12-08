(****
    Day 8
      *****)
open Core
open Aoc2022_lib

let sight_lines g x y =
  let get_on_col x y = IntGrid.get g x y in
  let get_on_row y x = IntGrid.get g x y in
  let left = List.map (List.rev (List.range 0 x)) ~f:(get_on_row y) in
  let top = List.map (List.rev (List.range 0 y)) ~f:(get_on_col x) in
  let right =
    List.map (List.range (x + 1) (IntGrid.width g)) ~f:(get_on_row y)
  in
  let bot =
    List.map (List.range (y + 1) (IntGrid.height g)) ~f:(get_on_col x)
  in
  [ left; top; right; bot ]

let is_visible_global g x y =
  let v = IntGrid.get g x y in
  let is_vis_slice = List.for_all ~f:(fun x -> x < v) in
  sight_lines g x y |> List.map ~f:is_vis_slice
  |> List.fold ~init:false ~f:( || )

let count_visible g =
  IntGrid.positions g |> List.count ~f:(fun (x, y) -> is_visible_global g x y)

let scenic_score g x y =
  let v = IntGrid.get g x y in
  let num_vis_in_line v sl =
    let smaller = List.take_while sl ~f:(fun x -> x < v) in
    let boundary = List.drop_while sl ~f:(fun x -> x < v) in
    let num_smaller = List.length smaller in
    if List.is_empty boundary then num_smaller else num_smaller + 1
  in
  sight_lines g x y
  |> List.map ~f:(num_vis_in_line v)
  |> List.fold ~init:1 ~f:( * )

let best_scenic_score g =
  IntGrid.positions g
  |> List.map ~f:(fun (x, y) -> scenic_score g x y)
  |> List.max_elt ~compare:Int.compare
  |> Option.value ~default:0

let solve params lines =
  let g = IntGrid.from_lines lines in
  match params.(1) |> int_of_string with
  | 1 -> count_visible g
  | 2 -> best_scenic_score g
  | _ -> raise @@ Failure "invalid part"