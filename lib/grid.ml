open Core
open Util

type t = int Array.t Array.t

let get_exn g ~col ~row = g.(row).(col)
let gett_exn g ~pos = g.(snd pos).(fst pos)
let get g ~col ~row = try Some g.(row).(col) with _e -> None
let set g ~col ~row v = g.(row).(col) <- v

let from_lines ~f lines =
  let row_of_line line = Array.of_list (List.map ~f (String.to_list line)) in
  Array.of_list (List.map ~f:row_of_line lines)

let allocate ~height ~width ~default =
  Array.of_list (repeat ~num:height (Array.create ~len:width default))

let height = Array.length
let width g = Array.length g.(0)

let valid_pos g ~col ~row =
  0 <= col && col < width g && 0 <= row && row < height g

let positions g =
  List.concat_map
    ~f:(fun row -> List.map ~f:(fun col -> (col, row)) (List.range 0 (width g)))
    (List.range 0 (height g))

let find g ~f =
  positions g |> List.find ~f:(fun (col, row) -> f (get_exn g ~col ~row))

let cardinal_neighbors g (col, row) =
  List.filter_map
    ~f:(fun (dx, dy) ->
      let col', row' = (col + dx, row + dy) in
      if valid_pos g ~col:(col + dx) ~row:(row + dy) then Some (col', row')
      else None)
    [ (-1, 0); (0, 1); (1, 0); (0, -1) ]
