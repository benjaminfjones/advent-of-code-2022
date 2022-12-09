open Core

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

(* L2 norm (squared) *)
let dist2 (x, y) (v, w) = ((x - v) * (x - v)) + ((y - w) * (y - w))

(* L_infinity norm *)
let dist_inf (x, y) (v, w) = Int.max (Int.abs (x - v)) (Int.abs (y - w))

let neighbors (x, y) =
  [
    (x + 1, y + 1);
    (x + 1, y);
    (x + 1, y - 1);
    (x, y + 1);
    (x, y);
    (x, y - 1);
    (x - 1, y + 1);
    (x - 1, y);
    (x - 1, y - 1);
  ]

let min_one_step_energy (current : vec2) (target : vec2) : vec2 =
  let candidate_moves = neighbors current in
  let dists = List.map candidate_moves ~f:(fun v -> (v, dist2 target v)) in
  fst
    (Option.value_exn
       (List.min_elt dists ~compare:(fun (_, d1) (_, d2) -> Int.compare d1 d2)))

let chase (current : vec2) (target : vec2) : vec2 =
  match dist_inf current target with
  | 0 | 1 -> current
  | 2 -> min_one_step_energy current target
  | _ -> failwith "rope broke!"

type move = L | R | U | D

let parse_dir = function
  | "L" -> Some L
  | "R" -> Some R
  | "U" -> Some U
  | "D" -> Some D
  | _ -> None

let repeat ~(num : int) elt = List.map (List.range 0 num) ~f:(fun _ -> elt)

type state = { knots : vec2 list; trace : vec2 list }

let initial_state_p1 = { knots = [ zero; zero ]; trace = [ zero ] }
let initial_state_p2 = { knots = repeat ~num:10 zero; trace = [ zero ] }

let move_knot m kt =
  match m with
  | L -> kt +> (-1, 0)
  | R -> kt +> (1, 0)
  | U -> kt +> (0, 1)
  | D -> kt +> (0, -1)

let exec_move m { knots; trace } =
  let head' = move_knot m (List.hd_exn knots) in
  let rec aux kts_rev knots =
    match knots with
    | [] -> List.rev kts_rev
    | k :: rest -> aux (chase k (List.hd_exn kts_rev) :: kts_rev) rest
  in
  let knots' = aux [ head' ] (List.tl_exn knots) in
  let tail' = List.last_exn knots' in
  { knots = knots'; trace = tail' :: trace }

let exec_move_list ms st =
  List.fold ms ~init:st ~f:(fun st0 m0 ->
      let st' = exec_move m0 st0 in
      (* Printf.printf "h: %s, t: %s\n" (pp st'.head) (pp st'.tail); *)
      st')

let parse_line line =
  match String.split line ~on:' ' with
  | [ d; s ] -> (
      match parse_dir d with
      | Some d -> repeat ~num:(int_of_string s) d
      | None -> failwith ("parse error for line: " ^ line))
  | _ -> failwith ("parse error for line: " ^ line)

let parse_input = List.concat_map ~f:parse_line

let solve params lines =
  let moves = parse_input lines in
  match params.(1) |> int_of_string with
  | 1 ->
      let final_state = exec_move_list moves initial_state_p1 in
      List.length (List.dedup_and_sort ~compare:compare_vec2 final_state.trace)
  | 2 ->
      let final_state = exec_move_list moves initial_state_p2 in
      List.length (List.dedup_and_sort ~compare:compare_vec2 final_state.trace)
  | _ -> raise @@ Failure "invalid part"
