open Core
open Aoc2022_lib

type move =
    Rock
    | Paper
    | Scissors

let int_of_move = function
    | Rock     -> 0
    | Paper    -> 1
    | Scissors -> 2

let move_of_int = function
    | 0 -> Rock
    | 1 -> Paper
    | 2 -> Scissors
    | _ -> raise @@ Failure "invalid move int"

type outcome =
    Lose
    | Draw
    | Win

let score_outcome = function
    | Lose -> 0
    | Draw -> 3
    | Win -> 6

let score_move = function
    | Rock     -> 1
    | Paper    -> 2
    | Scissors -> 3

let move_of_code = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> raise @@ Failure "invalid code"

type round = move * move

let outcome_of_round (r: round) : outcome =
    let (iom, imm) = (int_of_move (fst r), int_of_move (snd r)) in
    match imm - iom with
    | 0 -> Draw
    | 1 -> Win
    | -1 -> Lose
    | 2 -> Lose
    | -2 -> Win
    | _ -> raise @@ Failure "impossible"

let score_round (r: round) : int =
    score_outcome (outcome_of_round r) + score_move (snd r)

let parse_input (lines: string list) : (move * move) list =
    let line_parser (line: string) : move * move = String.split ~on:' ' line
        |> List.map ~f:move_of_code
        |> (fun ms -> (List.nth_exn ms 0, List.nth_exn ms 1))
    in List.map ~f:line_parser lines

let score_tournament rounds =
    int_sum (List.map ~f:score_round rounds)

let decrypt_xyz = function
    | Rock -> Lose
    | Paper -> Draw
    | Scissors -> Win

let decrypt_round (r: round) : round =
    let iom = int_of_move (fst r) in
    match decrypt_xyz (snd r) with
    | Lose -> (fst r, move_of_int ((iom - 1) % 3))
    | Draw -> (fst r, fst r)
    | Win -> (fst r, move_of_int ((iom + 1) % 3))

let solve params lines =
    let rounds = parse_input lines in
  match params.(1) |> int_of_string with
  | 1 -> score_tournament rounds
  | 2 -> score_tournament (List.map ~f:decrypt_round rounds)
  | _ -> raise @@ Failure "invalid part"

