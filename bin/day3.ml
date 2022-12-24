open Core
open Lib.Util

type ruck = int list

(* a..z -> 1..26, A..Z -> 27..52 *)
let char_to_int c =
  if Char.is_lowercase c then Char.to_int c - Char.to_int 'a' + 1
  else Char.to_int c - Char.to_int 'A' + 27

let split_line (line : string) : ruck =
  List.map ~f:char_to_int (String.to_list line)

let parse_input = List.map ~f:split_line

(* split a ruck into two halfs *)
let split_ruck (r : ruck) : ruck * ruck =
  let l = List.length r in
  (List.take r (l / 2), List.drop r (l / 2))

(* intersect the two halfs of a ruck *)
let inter_halfs (h1, h2) =
  Set.to_list
    (Set.inter (Set.of_list (module Int) h1) (Set.of_list (module Int) h2))

(* intersect a list of rucks *)
let inter_rucks rs =
  match rs with
  | [] -> []
  | r :: rest ->
      let mkset = Set.of_list (module Int) in
      Set.to_list
        (List.fold rest ~init:(mkset r) ~f:(fun x y -> Set.inter x (mkset y)))

let solve params lines =
  let rucks = parse_input lines in
  match params.(1) |> int_of_string with
  | 1 ->
      let find_common_item r = split_ruck r |> inter_halfs in
      sum_ints (List.concat_map ~f:find_common_item rucks)
  | 2 ->
      let threes = List.chunks_of ~length:3 rucks in
      sum_ints (List.concat_map ~f:inter_rucks threes)
  | _ -> raise @@ Failure "invalid part"
