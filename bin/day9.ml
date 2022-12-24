open Core
open Lib.Util

type vec2 = int * int

let pp (x, y) = Format.sprintf "(%d, %d)" x y

let compare_vec2 (x, y) (v, w) =
  match Int.compare x v with
  | 1 -> 1
  | -1 -> -1
  | 0 -> Int.compare y w
  | _ -> failwith "impossible"

let zero = (0, 0)
let ( +> ) (x, y) (v, w) = (x + v, y + w)

(* L2 norm (squared)
 *
 * Note: minimizing L2^2 is equivalent to minimizing L2 and lets us avoid
 * doing any floating point arithmetic.
 *)
let dist2 (x, y) (v, w) = ((x - v) * (x - v)) + ((y - w) * (y - w))

(* L_infinity norm *)
let dist_inf (x, y) (v, w) = Int.max (Int.abs (x - v)) (Int.abs (y - w))

(* Returns all vectors at most one step from the input, including the input itself *)
let neighbors v =
  let delta1d = [ -1; 0; 1 ] in
  let delta2d = List.cartesian_product delta1d delta1d in
  List.map ~f:(fun d -> v +> d) delta2d

(*
 * Among all possible one-step moves for `current`, select the one that
 * minimizes energy. The energy model is based on the ideal spring model
 * (Hooke's Law).
 *)
let min_one_step_energy (current : vec2) (target : vec2) : vec2 =
  let candidate_moves = neighbors current in
  let dists = List.map candidate_moves ~f:(fun v -> (v, dist2 target v)) in
  fst
    (Option.value_exn
       (List.min_elt dists ~compare:(fun (_, d1) (_, d2) -> Int.compare d1 d2)))

(*
  * Determine the next position for `current` in the physical siimulation.
  *
  * If `current` is adjecent to `target`, the rope is not under tension, so
  * there is no force and hence no movement. If `target` is further away, determine
  * the next one-step away postion for current that minimizes system energy.
  *)
let chase (current : vec2) (target : vec2) : vec2 =
  match dist_inf current target with
  | 0 | 1 -> current
  | 2 -> min_one_step_energy current target
  | _ -> failwith "rope broke!"

(* Cardinal directions for moves: (L)eft, (R)ight, (U)p, (D)own *)
type move = L | R | U | D

let parse_move = function
  | "L" -> Some L
  | "R" -> Some R
  | "U" -> Some U
  | "D" -> Some D
  | _ -> None

(* Move the knot in the given cardinal direction *)
let move_knot m kt =
  match m with
  | L -> kt +> (-1, 0)
  | R -> kt +> (1, 0)
  | U -> kt +> (0, 1)
  | D -> kt +> (0, -1)

(* `state` tracks a list of knots, with the head at the head of the list
   and the tail at the end. The `trace` tracks all values
   of the tail over time.
*)
type state = { knots : vec2 list; trace : vec2 list }

let exec_move m { knots; trace } =
  (*
     let knots = [knot0; knot1; ...]
         knot0' = move_knot knot0
         knoti' = chase knoti knot{i-1}'
     then,

     aux [knot0'] [knot1; knot2; ...]
     aux [knot1'; knot0'] [knot2; ...]
     ...
     aux [knotN'; knot{N-1}'; ...] [] -> reverse
     [knot0'; knot1'; ...; knotN']
  *)
  let head' = move_knot m (List.hd_exn knots) in
  (* TODO: replace `aux` with a fold *)
  let rec aux kts_rev knots =
    match knots with
    | [] -> List.rev kts_rev
    | k :: rest -> aux (chase k (List.hd_exn kts_rev) :: kts_rev) rest
  in
  let knots' = aux [ head' ] (List.tl_exn knots) in
  let tail' = List.last_exn knots' in
  { knots = knots'; trace = tail' :: trace }

let exec_move_list ms st =
  List.fold ms ~init:st ~f:(fun st0 m0 -> exec_move m0 st0)

let parse_line line =
  match String.split line ~on:' ' with
  | [ d; s ] -> (
      match parse_move d with
      | Some d -> repeat ~num:(int_of_string s) d
      | None -> failwith ("parse error for line: " ^ line))
  | _ -> failwith ("parse error for line: " ^ line)

let parse_input = List.concat_map ~f:parse_line

let solve params lines =
  let moves = parse_input lines in
  match params.(1) |> int_of_string with
  | 1 ->
      (* A rope with 2 knots: a head and a tail *)
      let initial_state = { knots = [ zero; zero ]; trace = [ zero ] } in
      let final_state = exec_move_list moves initial_state in
      List.length (List.dedup_and_sort ~compare:compare_vec2 final_state.trace)
  | 2 ->
      (* A rope with 10 knots *)
      let initial_state =
        { knots = repeat ~num:10 zero; trace = [ zero ] }
      in
      let final_state = exec_move_list moves initial_state in
      List.length (List.dedup_and_sort ~compare:compare_vec2 final_state.trace)
  | _ -> raise @@ Failure "invalid part"
