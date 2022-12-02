open Core
open Aoc2022_lib

let parse_input lines =
  let proc acc x =
    match x with
    | "" -> [] :: acc
    | s -> (
        let num = Int.of_string s in
        match acc with
        | [] -> raise (Failure "unexpected empty acc")
        | hd :: tl -> (num :: hd) :: tl)
  in
  List.fold_left lines ~init:[ [] ] ~f:proc

let largest_sum lss =
  let sums = List.map ~f:int_sum lss in
  match int_max sums with
  | None -> raise @@ Failure "unexpected empty sums"
  | Some s -> s

let largest_three_sums lss =
  let sums = List.map ~f:int_sum lss in
  let sorted = List.rev (List.sort sums ~compare:Int.compare) in
  int_sum (List.slice sorted 0 3)

let solve params lines =
  let lss = parse_input lines in
  match params.(1) |> int_of_string with
  | 1 -> largest_sum lss
  | 2 -> largest_three_sums lss
  | _ -> raise @@ Failure "invalid part"

