open Core

type t = Vec.t [@@deriving sexp, compare]
(** Position in a 2d integer lattice.

   The position type is distinct from the 2d vector type.
   Representation is: x (col), y (row)
*)

let of_ = Vec.of_
let to_ = Vec.to_
let rel ~origin p = Vec.(subtract p origin)
let translate p v = Vec.add p v

let join p1 p2 =
  let x, y = Vec.to_ p1 and z, w = Vec.to_ p2 in
  Vec.of_ (max x z, max y w)

let meet p1 p2 =
  let x, y = Vec.to_ p1 and z, w = Vec.to_ p2 in
  Vec.of_ (min x z, min y w)

let less_eq p1 p2 =
  let (dx, dy) = Vec.to_ (rel ~origin:p1 p2) in
  dx >= 0 && dy >= 0

let pp p =
  let x, y = Vec.to_ p in
  Printf.sprintf "(%d, %d)" x y

let%test_unit "join" =
  [%test_eq: t] (join (of_ (1, 0)) (of_ (0, 1))) (of_ (1, 1))

let%test_unit "meet" =
  [%test_eq: t] (meet (of_ (1, 0)) (of_ (0, 1))) (of_ (0, 0))

let%test_unit "compare is lexico" =
  [%test_eq: int] (compare (of_ (0, 5)) (of_ (1, 3))) (-1)

let%test_unit "compare is lexico 2" =
  [%test_eq: int] (compare (of_ (1, 1)) (of_ (1, -1))) 1

let%test_unit "less_eq is not lexico" =
  [%test_eq: bool] (less_eq (of_ (0, 5)) (of_ (1, 3))) false

let%test_unit "less_eq is not lexico 2" =
  [%test_eq: bool] (less_eq (of_ (1, 1)) (of_ (1, -1))) false

let%test_unit "less_eq is not lexico 3" =
  [%test_eq: bool] (less_eq (of_ (1, 1)) (of_ (3, 9))) true

