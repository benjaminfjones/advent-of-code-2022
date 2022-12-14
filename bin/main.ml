open Core
open Lib

let solvers : (module Solver) Array.t =
  [|
    (module Day1);
    (module Day2);
    (module Day3);
    (module Day4);
    (module Day5);
    (module Day6);
    (module Day7);
    (module Day8);
    (module Day9);
    (module Day10);
    (module Day11);
    (module Day12);
    (module Day13);
    (module Day14);
  |]

let () =
  (* TODO: add debug logging
     if Sys.getenv_opt "DEBUG" |> Option.is_some then
       ( Logs.set_reporter (Logs_fmt.reporter ())
       ; Logs.set_level (Some Logs.Debug));
     Logs.debug (fun f -> f "Logging enabled");
  *)
  match Sys.getenv "AOC_DAY" with
  | None ->
      print_endline "Missing AOC_DAY env var";
      exit 1
  | Some day -> (
      match Array.get solvers (int_of_string day - 1) with
      | (exception Invalid_argument _) | (exception Failure _) ->
          print_endline ("Invalid AOC_DAY day: " ^ day);
          exit 1
      | (module Solver) ->
          In_channel.input_all In_channel.stdin
          |> String.split_lines
          |> Solver.solve @@ Sys.get_argv ()
          |> Printf.printf "%d\n")
