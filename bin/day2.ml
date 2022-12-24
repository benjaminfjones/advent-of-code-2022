open Core
open Lib.Util

type move = Rock | Paper | Scissors

(* The type `move` with `succ` and `pred` is isomorphic to Z/3Z with addition
 * and subtraction by 1 mod 3 *)
let succ = function Rock -> Paper | Paper -> Scissors | Scissors -> Rock
let pred = function Rock -> Scissors | Paper -> Rock | Scissors -> Paper

let move_equal m0 m1 =
  match (m0, m1) with
  | Rock, Rock -> true
  | Paper, Paper -> true
  | Scissors, Scissors -> true
  | _ -> false

type outcome = Lose | Draw | Win

let score_outcome = function Lose -> 0 | Draw -> 3 | Win -> 6
let score_move = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let move_of_code = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | "X" -> Rock
  | "Y" -> Paper
  | "Z" -> Scissors
  | _ -> raise @@ Failure "invalid code"

type round = move * move

let outcome_of_round (r : round) : outcome =
  let other_move, my_move = r in
  if move_equal (succ other_move) my_move then Win
  else if move_equal (succ my_move) other_move then Lose
  else Draw

let score_round (r : round) : int =
  score_outcome (outcome_of_round r) + score_move (snd r)

let parse_input (lines : string list) : (move * move) list =
  let line_parser (line : string) : move * move =
    String.split ~on:' ' line |> List.map ~f:move_of_code |> fun ms ->
    (List.nth_exn ms 0, List.nth_exn ms 1)
  in
  List.map ~f:line_parser lines

let score_tournament rounds = sum_ints (List.map ~f:score_round rounds)
let decrypt_xyz = function Rock -> Lose | Paper -> Draw | Scissors -> Win

let decrypt_round (om, mm) =
  match decrypt_xyz mm with
  | Lose -> (om, pred om)
  | Draw -> (om, om)
  | Win -> (om, succ om)

let solve params lines =
  let rs = parse_input lines in
  match params.(1) |> int_of_string with
  | 1 -> score_tournament rs
  | 2 -> score_tournament (List.map ~f:decrypt_round rs)
  | _ -> raise @@ Failure "invalid part"
