open Core

module type Solver = sig
  val solve : string Array.t -> string list -> int
end

let sum_ints = List.sum (module Int) ~f:(fun x -> x)
let max_ints = List.max_elt ~compare:Int.compare

(***********
 * IntRange
 ***********)

module IntRange : sig
  (* type representing an inclusive range of ints *)
  type t

  (* is an integer in the range? *)
  val in_range : int -> t -> bool

  (* do two ranges overlap? *)
  val overlaps : t -> t -> bool

  (* is the first range completely contained in the second? *)
  val contains : sub:t -> super:t -> bool

  (* parse a string into a range *)
  val of_string : string -> t
end = struct
  type t = { lower : int; upper : int }

  let in_range (x : int) (r : t) : bool = r.lower <= x && x <= r.upper

  let overlaps (r1 : t) (r2 : t) : bool =
    (* r1.lower in r2 *)
    in_range r1.lower r2
    (* r1.upper in r2 *)
    || in_range r1.upper r2
    (* neither r1.lower or r1.upper in r2 <=> r2.lower in r1 *)
    || in_range r2.lower r1

  let contains ~(sub : t) ~(super : t) : bool =
    in_range sub.lower super && in_range sub.upper super

  let of_string s =
    match String.split s ~on:'-' with
    | [ ls; us ] -> { lower = Int.of_string ls; upper = Int.of_string us }
    | _ -> raise @@ Failure ("failed to parse range: " ^ s)
end