open Core
open Lib
open Day14lib

let build_cave rockpaths =
  List.fold rockpaths
    ~init:Cave.(empty initial_particle)
    ~f:(fun acc_cave rp -> Cave.add_rocks rp acc_cave)

let build_cave_with_floor rockpaths =
  let initial_cave = build_cave rockpaths in
  let floor_y = snd (Pos.to_ initial_cave.bounds.upper) + 2 in
  (* 500 +- floor_y suffices for the sand to pile high enough to block the
     entrance *)
  let floor_left = Pos.of_ (500 - floor_y, floor_y)
  and floor_right = Pos.of_ (500 + floor_y, floor_y) in
  Cave.add_rock_line ~start_p:floor_left ~end_p:floor_right initial_cave

let parse_line line =
  let positions = String.split line ~on:' ' in
  let rec loop ps =
    match ps with
    | [] -> []
    | "->" :: rest -> loop rest
    | ps_str :: rest -> (
        match String.split ps_str ~on:',' with
        | [ x_str; y_str ] ->
            Pos.of_ (int_of_string x_str, int_of_string y_str) :: loop rest
        | _ -> failwith ("parse error: " ^ ps_str))
  in
  loop positions

let solve params lines =
  let rockpaths = List.map ~f:parse_line lines in
  match params.(1) |> int_of_string with
  | 1 ->
      let final_state =
        run
          { cave = build_cave rockpaths; particle = initial_particle; step = 0 }
          (* fuel was determined empirically using the puzzle input *)
          200000
      in
      let amount_sand = Cave.count_sand final_state.cave in
      print_endline (pp_state final_state);
      Printf.printf "amount of sand = %d\n" amount_sand;
      Printf.printf "simulation steps = %d\n" final_state.step;
      amount_sand
  | 2 ->
      let final_state =
        run
          {
            cave = build_cave_with_floor rockpaths;
            particle = initial_particle;
            step = 0;
          }
          (* fuel was determined empirically using the puzzle input *)
          5000000
      in
      let amount_sand = Cave.count_sand final_state.cave in
      print_endline (pp_state final_state);
      Printf.printf "amount of sand = %d\n" amount_sand;
      Printf.printf "simulation steps = %d\n" final_state.step;
      amount_sand
  (* "part 10" is a terminal animation of the sand falling into the cave *)
  | 10 ->
      let i = ref 0 in
      let state =
        ref
          { cave = build_cave rockpaths; particle = initial_particle; step = 0 }
      in
      while !i < 200000 && particle_inbounds !state do
        Printf.printf "\027[2J\027[H";
        Printf.printf "step %d\n\n" !i;
        print_endline (pp_state ~color:true !state);
        Caml_unix.sleepf 0.01;
        state := run !state 1;
        i := !i + 1
      done;
      0
  | _ -> raise @@ Failure "invalid part"
