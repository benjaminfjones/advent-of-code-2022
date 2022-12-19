open Core
open Aoc2022_lib

(* 2d positions *)

type position = int * int

let position_equal (x, y) (z, w) = x = z && y = w
let dist2 (x, y) (z, w) = ((x - z) * (x - z)) + ((y - w) * (y - w))
let dist_inf (x, y) (z, w) = abs (x - z) + abs (y - w)
let pp_pos (col, row) = Printf.sprintf "(%d,%d)" col row
let pp_path (path : position list) : string =
     List.map path ~f:pp_pos
     |> String.concat ~sep:"; "
let num_steps (path: position list) : int = (List.length path) - 1

(* Topomap is a 2d grid of positive integer heights *)
type topomap = { start : position; end_ : position; grid : IntGrid.t }

let can_move topo old_pos new_pos =
  IntGrid.gett_exn topo.grid ~pos:new_pos
  - IntGrid.gett_exn topo.grid ~pos:old_pos
  <= 1

(*
 * Global best-first search, i.e. some form of A*.
 *)

module Position = struct
  type t = int * int [@@deriving sexp, compare]
end

module PosSet = Set.Make (Position)
module PosMap = Map.Make (Position)

let uphill_neighbors topo pos =
  IntGrid.cardinal_neighbors topo.grid pos |> List.filter ~f:(can_move topo pos)

let reconstruct_path (came_from : position PosMap.t) (current : position) :
    position list =
  let rec loop result =
    let cur = List.hd_exn result in
    match PosMap.find came_from cur with
    | Some prev ->
      loop (prev :: result)
    | None -> result
  in
  loop [ current ]

type astar_state = {
  openset : PosSet.t;
  camefrom : position PosMap.t;
  gscore : int PosMap.t;
  fscore : int PosMap.t;
}

(*
 * Return the node in the open set with minimum fscore.
 *
 * Assumes: every element of st.openset is a key in st.fscore
 * TODO: replace linear scan with paired-heap priority queue.
 *)
let find_best (st : astar_state) : position option =
  PosSet.to_list st.openset
  |> List.min_elt ~compare:(fun p1 p2 ->
         Int.compare
           (PosMap.find_exn st.fscore p1)
           (PosMap.find_exn st.fscore p2))

let astar (topo : topomap) : (position list) option =
  let hscore pos = dist_inf topo.end_ pos in
  (* gscore = min path length to each position so far under consideration.
     Global upper bound on path lengths is the number of positions in the
     grid. *)
  let global_max_gscore =
    (IntGrid.height topo.grid * IntGrid.width topo.grid) + 1
  in
  let gscore st pos =
    PosMap.find st.gscore pos |> Option.value ~default:global_max_gscore
  in
  let init_state =
    {
      openset = PosSet.singleton topo.start;
      camefrom = PosMap.empty;
      gscore = PosMap.(empty |> set ~key:topo.start ~data:0);
      fscore = PosMap.(empty |> set ~key:topo.start ~data:(hscore topo.start));
    }
  in
  let rec loop (main_loop_state : astar_state) : (position list) option =
    if PosSet.is_empty main_loop_state.openset then
      (* DEBUG *)
      (* (Printf.printf "openset is empty without reaching goal: start=%s\n" *)
      (* (pp_pos topo.start); None) *)
      None
    else
      let current = Option.value_exn (find_best main_loop_state) in
      (* DEBUG *)
      (* Printf.printf "path: %s\n" *)
      (*   (pp_path (reconstruct_path main_loop_state.camefrom current)); *)
      if position_equal current topo.end_ then
        Some (reconstruct_path main_loop_state.camefrom current)
      else
        (* remove current from openset *)
        let main_loop_state' =
          {
            main_loop_state with
            openset = PosSet.remove main_loop_state.openset current;
          }
        in
        (* graph weight from current -> nb is always 1 *)
        let gscore_from_current =
          PosMap.find_exn main_loop_state'.gscore current + 1
        in
        let neighbors = uphill_neighbors topo current in
        let rec nb_loop nb_loop_state nbs =
          match nbs with
          | [] -> nb_loop_state
          | nb :: rest ->
              if gscore_from_current < gscore nb_loop_state nb then
                let openset' =
                  if PosSet.mem nb_loop_state.openset nb then
                    nb_loop_state.openset
                  else PosSet.add nb_loop_state.openset nb
                in
                let camefrom' =
                  PosMap.set nb_loop_state.camefrom ~key:nb ~data:current
                in
                let gscore' =
                  PosMap.set nb_loop_state.gscore ~key:nb
                    ~data:gscore_from_current
                in
                let fscore' =
                  PosMap.set nb_loop_state.fscore ~key:nb
                    ~data:(gscore_from_current + hscore nb)
                in
                nb_loop
                  {
                    openset = openset';
                    camefrom = camefrom';
                    gscore = gscore';
                    fscore = fscore';
                  }
                  rest
              else nb_loop nb_loop_state rest
        in
        let main_loop_state'' = nb_loop main_loop_state' neighbors in
        loop main_loop_state''
  in
  loop init_state

(*
 * Depth first search with pruning heuristics, and local best-first search
 * These are insufficient to solve the puzzle.
 *)

type moment = { current_pos : position; to_visit : position list }

let moments_to_path = List.map ~f:(fun m -> m.current_pos)

let closer_to_goal topo p1 p2 =
  Int.compare (dist2 p1 topo.end_) (dist2 p2 topo.end_)

let higher topo p1 p2 =
  Int.compare
    (IntGrid.gett_exn topo.grid ~pos:p2)
    (IntGrid.gett_exn topo.grid ~pos:p1)

let higher_and_closer topo p1 p2 =
  let hc =
    Int.compare
      (IntGrid.gett_exn topo.grid ~pos:p2)
      (IntGrid.gett_exn topo.grid ~pos:p1)
  in
  if hc <> 0 then hc else closer_to_goal topo p1 p2

let admissible_neighbors topo pos visited =
  (* all *)
  IntGrid.cardinal_neighbors topo.grid pos
  (* movable *)
  |> List.filter ~f:(can_move topo pos)
  (* unvisited and movable *)
  |> List.filter ~f:(fun p -> not (List.mem visited p ~equal:position_equal))
  (* Local best-first heuristic *)
  |> List.sort ~compare:(higher_and_closer topo)

type dfsstats = {
  num_backtracks : int;
  num_prunes : int;
  num_goals : int;
  min_goal_length : int option;
}

let init_stats =
  { num_backtracks = 0; num_prunes = 0; num_goals = 0; min_goal_length = None }

let pp_stats stats =
  Printf.sprintf "backtracks=%d prunes=%d goals=%d mingoallen=%d"
    stats.num_backtracks stats.num_prunes stats.num_goals
    (Option.value stats.min_goal_length ~default:(-1))

(* Depth first search over a 2d grid with hill-climbing constraints

   This search algorithm fails to find any path from start to end over
   the real puzzle input height map in a reasonable amount of time.

   Performance notes: this implementation acheives roughly 335k backtracks per
   second.

   Stats for test puzzle input:
   next-to-goal pruning: num_backtracks=1395, num_goals=30
   min-path-length pruning: backtracks=1289 prunes=22 goals=8 mingoallen=32
   close-to-goal sorting: backtracks=1280 prunes=23 goals=7 mingoallen=32
   higher_and_closer-to-goal sorting: backtracks=1280 prunes=23 goals=7 mingoallen=32
*)
let dfs (topo : topomap) ~(prune_to_shortest : bool) : position list list =
  let rec aux moments results stats =
    (* DEBUG *)
    (* Printf.printf "%s\n" (pp_stats stats); *)
    match moments with
    | [] -> (results, stats)
    | cur_moment :: rest_moments -> (
        (* DEBUG *)
        (* Printf.printf "path: %s\n" (pp_path (moments_to_path moments)); *)
        match cur_moment.to_visit with
        (* backtrack it up! *)
        | [] ->
            aux rest_moments results
              { stats with num_backtracks = stats.num_backtracks + 1 }
        (* visit next grid position *)
        | next :: rest_to_visit ->
            let visited = moments_to_path moments in
            if
              prune_to_shortest
              && Option.is_some stats.min_goal_length
              && Option.value_exn stats.min_goal_length <= List.length visited
            then
              aux rest_moments results
                { stats with num_prunes = stats.num_prunes + 1 }
            else if position_equal next topo.end_ then
              (* if next is the end, push a new result path *)
              let goal_path_length = List.length visited + 1 in
              let min_goal_length' =
                match stats.min_goal_length with
                | None -> Some goal_path_length
                | Some l when l > goal_path_length -> Some goal_path_length
                | _ -> stats.min_goal_length
              in
              let moments' =
                if prune_to_shortest then rest_moments
                else
                  { cur_moment with to_visit = rest_to_visit } :: rest_moments
              in
              let results' = (next :: visited) :: results in
              aux moments' results'
                {
                  stats with
                  num_goals = stats.num_goals + 1;
                  min_goal_length = min_goal_length';
                }
            else
              (* if next up is not the end, push a new moment for next and recurse *)
              let next_to_visit = admissible_neighbors topo next visited in
              let moments' =
                { current_pos = next; to_visit = next_to_visit }
                :: { cur_moment with to_visit = rest_to_visit }
                :: rest_moments
              in
              aux moments' results stats)
  in
  let results, final_stats =
    aux
      [
        {
          current_pos = topo.start;
          to_visit = admissible_neighbors topo topo.start [];
        };
      ]
      [] init_stats
  in
  Printf.printf "summary: %s\n" (pp_stats final_stats);
  results

let alpha_to_height = function
  | 'S' -> -1
  | 'E' -> -2
  | c -> Char.to_int c - Char.to_int 'a'

(*
 * Parse a slightly generalized form of the input to allow for maps with more
 * flexible start and end heights
 *)
let parse_input (lines : string list) : topomap =
  match lines with
  | [] -> failwith "emtpy input lines"
  | first :: rest -> (
      match String.split first ~on:' ' with
      | [ "start"; s; "end"; e ] -> (
          let start_height = int_of_string s in
          let end_height = int_of_string e in
          let g = IntGrid.from_lines ~f:alpha_to_height rest in
          let mstart = IntGrid.find g ~f:(fun h -> h = -1) in
          let mend = IntGrid.find g ~f:(fun h -> h = -2) in
          match (mstart, mend) with
          | Some start, Some end_ ->
              IntGrid.set g ~col:(fst start) ~row:(snd start) start_height;
              IntGrid.set g ~col:(fst end_) ~row:(snd end_) end_height;
              { start; end_; grid = g }
          | _ -> failwith "failed to find start and/or end")
      | _ -> failwith "could not parse first start/end height line")

let solve params lines =
  let topo = parse_input lines in
  match params.(1) |> int_of_string with
  | 1 -> (
      match astar topo with
      | Some path -> num_steps path
      | None -> failwith "failed to solve part 1")
  | 2 ->
      let low_starting_topos =
        IntGrid.positions topo.grid
        |> List.filter ~f:(fun p -> (IntGrid.gett_exn topo.grid ~pos:p) = 0)
        |> List.map ~f:(fun p -> { topo with start=p }) in
      low_starting_topos
      |> List.filter_map ~f:(fun t -> (Option.map ~f:num_steps (astar t)))
      |> min_ints
      |> Option.value_exn
  (* "part 10" is the failed dfs with heuristics implementation *)
  | 10 ->
      dfs topo ~prune_to_shortest:true
      |> List.map ~f:(fun p -> List.length p - 1)
      |> min_ints |> Option.value ~default:0
  | _ -> raise @@ Failure "invalid part"
