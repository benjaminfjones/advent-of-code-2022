open Core
open Lib

let parse_input (lines : string list) : (Range.t * Range.t) list =
  let range_pair_of_line line =
    match String.split line ~on:',' with
    | [ p1; p2 ] -> (Range.of_string p1, Range.of_string p2)
    | _ -> raise @@ Failure ("failed to parse line: " ^ line)
  in
  List.map lines ~f:range_pair_of_line

let one_pair_in_other (p1, p2) =
  Range.contains ~sub:p1 ~super:p2 || Range.contains ~sub:p2 ~super:p1

let pairs_overlap (p1, p2) = Range.overlaps p1 p2

let solve params lines =
  let range_pairs = parse_input lines in
  match params.(1) |> int_of_string with
  | 1 -> List.count range_pairs ~f:one_pair_in_other
  | 2 -> List.count range_pairs ~f:pairs_overlap
  | _ -> raise @@ Failure "invalid part"
