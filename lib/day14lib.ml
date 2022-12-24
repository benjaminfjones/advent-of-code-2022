open Core

(*-- Bounding Box --------------------------------------------------------*)

module BoundingBox = struct
  type t = { lower : Pos.t; upper : Pos.t } [@@deriving sexp, compare]

  let singleton p = { lower = p; upper = p }

  (* Return the smallest bounding box including the given one and the
     position *)
  let update bb p = { lower = Pos.meet bb.lower p; upper = Pos.join bb.upper p }

  let smallest_containing bb1 bb2 =
    {
      lower = Pos.meet bb1.lower bb2.lower;
      upper = Pos.join bb1.upper bb2.upper;
    }

  let of_path (ps : Pos.t list) : t =
    match ps with
    | [] -> failwith "unexpected empty list"
    | p :: ps -> List.fold ps ~init:(singleton p) ~f:(fun bb p' -> update bb p')

  let contains bb p = Pos.less_eq bb.lower p && Pos.less_eq p bb.upper
end

(*-- Cave ----------------------------------------------------------------*)

type matter = Air | Rock | Sand [@@deriving sexp, compare]

let pp_matter = function Air -> "." | Rock -> "#" | Sand -> "o"

module PosMap = Map.Make (Pos)

module Cave = struct
  type t = { grid : matter PosMap.t; bounds : BoundingBox.t }
  [@@deriving sexp, compare]

  let empty seed_pos =
    { grid = PosMap.empty; bounds = BoundingBox.singleton seed_pos }

  let get cave p =
    match PosMap.find cave.grid p with Some m -> m | None -> Air

  let add_matter pos mat cave =
    {
      grid = PosMap.set cave.grid ~key:pos ~data:mat;
      bounds = BoundingBox.update cave.bounds pos;
    }

  let add_rock_line ~start_p ~end_p cave =
    let v = Pos.rel ~origin:start_p end_p in
    let dp, n =
      match Vec.to_ v with
      | 0, z -> (Vec.scale_inv v (abs z), abs z)
      | z, 0 -> (Vec.scale_inv v (abs z), abs z)
      | _ ->
          failwith
            ("invalid corners: start=" ^ Pos.pp start_p ^ ", end="
           ^ Pos.pp end_p)
    in
    List.(
      fold
        (range 0 (n + 1))
        ~init:cave
        ~f:(fun acc_cave i ->
          add_matter (Pos.translate start_p (Vec.scale dp i)) Rock acc_cave))

  let add_rocks corners cave =
    let rec loop_pairs ps tmp_cave =
      match ps with
      | [] -> tmp_cave
      | _ :: [] -> tmp_cave
      | p1 :: p2 :: rest ->
          loop_pairs (p2 :: rest) (add_rock_line ~start_p:p1 ~end_p:p2 tmp_cave)
    in
    loop_pairs corners cave

  let count_sand cave =
    PosMap.to_alist cave.grid
    |> List.filter ~f:(fun (_, m) -> match m with Sand -> true | _ -> false)
    |> List.length

  let pp ?(mark = None) cave =
    let x0, y0 = Pos.to_ cave.bounds.lower
    and x1, y1 = Pos.to_ cave.bounds.upper in
    let header =
      "    "
      ^ List.(
          range x0 (x1 + 1)
          |> map ~f:(fun x -> Printf.sprintf "%1d" (x % 10))
          |> String.concat)
      ^ "\n"
    in
    header
    ^ List.(
        range y0 (y1 + 1)
        |> map ~f:(fun y ->
               range x0 (x1 + 1)
               |> map ~f:(fun x ->
                      let p = Pos.of_ (x, y) in
                      match mark with
                      | Some m when Pos.compare m p = 0 -> "x"
                      | _ -> get cave p |> pp_matter)
               |> String.concat
               |> fun row -> Printf.sprintf "%3d %s\n" y row)
        |> String.concat)
end

(*-- Simulation ----------------------------------------------------------*)

let initial_particle = Pos.of_ (500, 0)
let out_of_bounds_particle = Pos.of_ (500, -1)

type state = { cave : Cave.t; particle : Pos.t; step : int } [@@deriving sexp]

let pp_state st = Cave.pp st.cave ~mark:(Some st.particle)

let next_pos p =
  List.map
    [ (0, 1); (-1, 1); (1, 1) ]
    ~f:(fun cs -> Pos.translate p (Vec.of_ cs))

let next st =
  let step' = st.step + 1 in
  let st' =
    match
      List.map (next_pos st.particle) ~f:(fun p -> (p, Cave.get st.cave p))
    with
    | (p', Air) :: _ -> { st with particle = p' }
    | _ :: (p', Air) :: _ -> { st with particle = p' }
    | _ :: _ :: (p', Air) :: _ -> { st with particle = p' }
    | _ ->
        let cave' = Cave.add_matter st.particle Sand st.cave in
        if Pos.compare st.particle initial_particle = 0 then
          (* when the entrance becomes blocked, kick the particle out of
             bounds *)
          { st with cave = cave'; particle = out_of_bounds_particle }
        else { st with cave = cave'; particle = initial_particle }
  in
  { st' with step = step' }

let particle_inbounds st = BoundingBox.contains st.cave.bounds st.particle

let run initial_state fuel =
  let rec loop st k =
    if k < fuel && particle_inbounds st then loop (next st) (k + 1) else st
  in
  loop initial_state 0

(*-- Tests ---------------------------------------------------------------*)

let%test_unit "bb update" =
  [%test_eq: BoundingBox.t]
    (BoundingBox.update
       (BoundingBox.singleton (Pos.of_ (0, 0)))
       (Pos.of_ (1, 1)))
    { lower = Pos.of_ (0, 0); upper = Pos.of_ (1, 1) }

let%test_unit "bb update 2" =
  [%test_eq: BoundingBox.t]
    (BoundingBox.update
       (BoundingBox.update
          (BoundingBox.singleton (Pos.of_ (0, 0)))
          (Pos.of_ (1, 1)))
       (Pos.of_ (-1, -1)))
    { lower = Pos.of_ (-1, -1); upper = Pos.of_ (1, 1) }

let test_bb_of_path =
  BoundingBox.of_path [ Pos.of_ (0, 0); Pos.of_ (2, 0); Pos.of_ (1, 3) ]

let test_bb = BoundingBox.{ lower = Pos.of_ (0, 0); upper = Pos.of_ (2, 3) }

let%test_unit "bb from path" = [%test_eq: BoundingBox.t] test_bb_of_path test_bb

let%test "bb contains" =
  BoundingBox.contains test_bb_of_path (Pos.of_ (0, 0))
  && BoundingBox.contains test_bb_of_path (Pos.of_ (1, 1))
  && BoundingBox.contains test_bb_of_path (Pos.of_ (0, 3))
  && BoundingBox.contains test_bb_of_path (Pos.of_ (2, 0))

let%test "bb doesn't contain" =
  (not (BoundingBox.contains test_bb_of_path (Pos.of_ (4, 0))))
  && (not (BoundingBox.contains test_bb_of_path (Pos.of_ (0, 4))))
  && not (BoundingBox.contains test_bb_of_path (Pos.of_ (-1, -1)))

let%expect_test "single rock line cave" =
  let cave =
    Cave.(
      empty (Pos.of_ (0, 0))
      |> add_rock_line ~start_p:(Pos.of_ (0, 0)) ~end_p:(Pos.of_ (3, 0)))
  in
  print_endline (Cave.pp cave);
  [%expect {|
      0123
    0 #### |}]

let%expect_test "single rock line cave with sand and air" =
  let cave =
    Cave.(
      empty (Pos.of_ (0, 0))
      |> add_rock_line ~start_p:(Pos.of_ (0, 2)) ~end_p:(Pos.of_ (3, 2))
      |> add_matter (Pos.of_ (2, 0)) Sand
      |> add_matter (Pos.of_ (5, 5)) Air)
  in
  print_endline (Cave.pp cave);
  [%expect
    {|
      012345
    0 ..o...
    1 ......
    2 ####..
    3 ......
    4 ......
    5 ...... |}]

(* 498,4 -> 498,6 -> 496,6 *)
(* 503,4 -> 502,4 -> 502,9 -> 494,9 *)
let%expect_test "test cave" =
  let cave =
    Cave.(
      empty (Pos.of_ (493, 0))
      |> add_rock_line ~start_p:(Pos.of_ (498, 4)) ~end_p:(Pos.of_ (498, 6))
      |> add_rock_line ~start_p:(Pos.of_ (498, 6)) ~end_p:(Pos.of_ (496, 6))
      |> add_rock_line ~start_p:(Pos.of_ (503, 4)) ~end_p:(Pos.of_ (502, 4))
      |> add_rock_line ~start_p:(Pos.of_ (502, 4)) ~end_p:(Pos.of_ (502, 9))
      |> add_rock_line ~start_p:(Pos.of_ (502, 9)) ~end_p:(Pos.of_ (494, 9))
      |> add_matter (Pos.of_ (500, 0)) Sand
      |> add_matter (Pos.of_ (495, 0)) Air
      |> add_matter (Pos.of_ (503, 12)) Air)
  in
  print_endline (Cave.pp cave);
  [%expect
    {|
       34567890123
     0 .......o...
     1 ...........
     2 ...........
     3 ...........
     4 .....#...##
     5 .....#...#.
     6 ...###...#.
     7 .........#.
     8 .........#.
     9 .#########.
    10 ...........
    11 ...........
    12 ........... |}]

let%expect_test "test cave add_rocks" =
  let cave =
    Cave.(
      empty (Pos.of_ (493, 0))
      |> add_rocks [ Pos.of_ (498, 4); Pos.of_ (498, 6); Pos.of_ (496, 6) ]
      |> add_rocks
           [
             Pos.of_ (503, 4);
             Pos.of_ (502, 4);
             Pos.of_ (502, 9);
             Pos.of_ (494, 9);
           ]
      |> add_matter (Pos.of_ (500, 0)) Sand
      |> add_matter (Pos.of_ (495, 0)) Air
      |> add_matter (Pos.of_ (503, 12)) Air)
  in
  print_endline (Cave.pp cave);
  [%expect
    {|
       34567890123
     0 .......o...
     1 ...........
     2 ...........
     3 ...........
     4 .....#...##
     5 .....#...#.
     6 ...###...#.
     7 .........#.
     8 .........#.
     9 .#########.
    10 ...........
    11 ...........
    12 ........... |}]

(*-- Simulation Tests --*)

let test_cave =
  Cave.(
    empty (Pos.of_ (493, 0))
    |> add_rocks [ Pos.of_ (498, 4); Pos.of_ (498, 6); Pos.of_ (496, 6) ]
    |> add_rocks
         [
           Pos.of_ (503, 4); Pos.of_ (502, 4); Pos.of_ (502, 9); Pos.of_ (494, 9);
         ]
    |> add_matter (Pos.of_ (500, 0)) Air)

let%expect_test "test cave simulation to step 35" =
  let final_state =
    run { cave = test_cave; particle = initial_particle; step = 0 } 35
  in
  print_endline (pp_state final_state);
  [%expect
    {|
      34567890123
    0 .......x...
    1 ...........
    2 ...........
    3 ...........
    4 .....#...##
    5 .....#...#.
    6 ...###...#.
    7 .......o.#.
    8 ......ooo#.
    9 .#########. |}]

let%expect_test "test cave simulation to end" =
  let final_state =
    run { cave = test_cave; particle = initial_particle; step = 0 } 200
  in
  print_endline (pp_state final_state);
  Printf.printf "amount of sand = %d\n" (Cave.count_sand final_state.cave);
  Printf.printf "simulation steps = %d\n" final_state.step;
  [%expect
    {|
        34567890123
      0 ...........
      1 ...........
      2 .......o...
      3 ......ooo..
      4 .....#ooo##
      5 ....o#ooo#.
      6 ...###ooo#.
      7 .....oooo#.
      8 ..o.ooooo#.
      9 .#########.

    amount of sand = 24
    simulation steps = 171 |}]
