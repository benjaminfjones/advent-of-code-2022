open Core

(* 2d integer vector *)
type t = int * int [@@deriving sexp, compare]

let of_coords (x, y) = (x, y)
let to_coords (x, y) = (x, y)
let add (x, y) (z, w) = (x + z, y + w)
let subtract (x, y) (z, w) = (x - z, y - w)
let scale (x, y) z = (x*z, y*z)
let dist2 (x, y) (z, w) = (x - z)*(x - z) + (y - w)*(y - w)
let dist_inf (x, y) (z, w) = abs (x - z) + abs (y - w)
let pp (x, y) = Printf.sprintf "<%d, %d>" x y
