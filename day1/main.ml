open Core

let ex1 = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

type inv = int list list

let parse_input inp =
  let lines = String.split_lines inp in
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

let int_sum = List.fold_left ~init:0 ~f:( + )

let int_max xs =
  match xs with
  | [] -> None
  | x0 :: rest0 ->
      let rec aux max_so_far xs =
        match xs with
        | [] -> max_so_far
        | x :: rest ->
            if x > max_so_far then aux x rest else aux max_so_far rest
      in
      Some (aux x0 rest0)

let largest_sum lss =
  let sums = List.map ~f:int_sum lss in
  int_max sums

let largest_three_sums lss =
  let sums = List.map ~f:int_sum lss in
  let sorted = List.rev (List.sort sums ~compare:Int.compare) in
  int_sum (List.slice sorted 0 3)

let%test_unit "parse ex1" =
  [%test_eq: int list list] (parse_input ex1)
    [
      [ 10000 ];
      [ 9000; 8000; 7000 ];
      [ 6000; 5000 ];
      [ 4000 ];
      [ 3000; 2000; 1000 ];
    ]

let%test_unit "ex1 solution" =
  [%test_eq: int option] (largest_sum (parse_input ex1)) (Some 24000)

let input1 = In_channel.read_all "./input1"

let%test_unit "day1 p1 solution" =
  [%test_eq: int option] (largest_sum (parse_input input1)) (Some 66616)

let%test_unit "day1 p2 solution" =
  [%test_eq: int] (largest_three_sums (parse_input input1)) 199172
