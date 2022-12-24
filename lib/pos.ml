open Core

(** Position in a 2d integer lattice.

   The position type is distinct from the 2d vector type.
   Representation is: x (col), y (row)
*)
type t = Vec.t

let rel p1 p2 = Vec.(subtract p1 p2)
let translate p v = Vec.add p v
let pp p = let (x, y) = Vec.to_coords p in Printf.sprintf "(%d, %d)" x y
