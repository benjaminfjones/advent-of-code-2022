open Core

(* Crates are represented using a (mutable) array of lists of chars. Each list
   represents a stack of crates with head of list being the top crate.
*)
type ship = { crates : char list array }
type move = { num : int; src : int; dst : int }

(* Execute one crate move

   If `is_crate_mover_9001` is true, multiple crates are picked up and put down in
   their original order. Otherwise, one crate is picked up at a time which
   reverses their order on the destination stack.
*)
let update_ship (s : ship) (m : move) (is_crate_mover_9001 : bool) =
  let orig_src = s.crates.(m.src) in
  let orig_dst = s.crates.(m.dst) in
  let in_order_payload = List.take orig_src m.num in
  let payload =
    if is_crate_mover_9001 then in_order_payload else List.rev in_order_payload
  in
  s.crates.(m.src) <- List.drop orig_src m.num;
  s.crates.(m.dst) <- List.append payload orig_dst

(* Execute all moves of crates on the given ship. *)
let bulk_move_crates moves ship is_crate_mover_9001 =
  let num_stacks = Array.length ship.crates in
  List.iter moves ~f:(fun m -> update_ship ship m is_crate_mover_9001);
  let top_crates =
    List.(range 0 num_stacks >>| fun i -> List.hd_exn ship.crates.(i))
  in
  String.of_char_list top_crates

(*
Initial crates from the test input:

ZN
MCD
P
*)
let test_ship = { crates = [| [ 'N'; 'Z' ]; [ 'D'; 'C'; 'M' ]; [ 'P' ] |] }

(*
Moves from the test input:

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
*)
let test_moves =
  [
    { num = 1; src = 1; dst = 0 };
    { num = 3; src = 0; dst = 2 };
    { num = 2; src = 1; dst = 0 };
    { num = 1; src = 0; dst = 1 };
  ]

(* Skip the crate parsing fun and just hard code the input ship's crates *)
let initial_ship =
  let raw =
    "LNWTD\nCPH\nWPHNDGMJ\nCWSNTQL\nPHCN\nTHNDMWQB\nMBRJGSL\nZNWGVBRT\nWGDNPL"
  in
  let lines = String.split_lines raw in
  let list_crates = List.map lines ~f:(fun l -> String.to_list l |> List.rev) in
  { crates = Array.of_list list_crates }

let parse_line line =
  match String.split_on_chars line ~on:[ ' ' ] with
  | [ "move"; num; "from"; src; "to"; dst ] ->
      {
        num = Int.of_string num;
        src = Int.of_string src - 1;
        dst = Int.of_string dst - 1;
      }
  | _ -> raise @@ Failure ("could not parse line: " ^ line)

let parse_input (lines : string list) : move list = List.map lines ~f:parse_line

let solve params lines =
  let moves = parse_input lines in
  let ship = initial_ship in
  (* let moves = test_moves in *)
  (* let ship = test_ship in *)
  match params.(1) |> int_of_string with
  | 1 ->
      Printf.printf "%s\n" @@ bulk_move_crates moves ship false;
      42
  | 2 ->
      Printf.printf "%s\n" @@ bulk_move_crates moves ship true;
      68
  | _ -> raise @@ Failure "invalid part"
