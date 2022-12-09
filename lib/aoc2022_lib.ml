open Core

module type Solver = sig
  val solve : string Array.t -> string list -> int
end

(* Random List enhancements *)

let sum_ints = List.sum (module Int) ~f:(fun x -> x)
let mult_ints = List.fold ~init:1 ~f:( * )
let max_ints = List.max_elt ~compare:Int.compare
let or_bools = List.fold ~init:false ~f:( || )
let and_bools = List.fold ~init:true ~f:( && )
let repeat ~(num : int) elt = List.map (List.range 0 num) ~f:(fun _ -> elt)

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

(***********
 * IntGrid
 ***********)

module IntGrid : sig
  (* type representing a non-empty 2d grid of ints *)
  type t

  (* get an element value from the grid; exn if position is out of bounds *)
  val get : t -> int -> int -> int

  (* Construct an IntGrid from a 2d array of digits.

     TODO: generalize this to arbitrary ints, split by some specified function
  *)
  val from_lines : string list -> t

  (* size in the y-direction; always >= 1 *)
  val height : t -> int

  (* size in the x-direction; always >= 1 *)
  val width : t -> int

  (* list of all positions in the grid *)
  val positions : t -> (int * int) list
end = struct
  type t = int Array.t Array.t

  let get g x y = g.(x).(y)

  let from_lines lines =
    let row_of_line line =
      Array.of_list
        (List.map
           ~f:(fun c -> int_of_string (Char.to_string c))
           (String.to_list line))
    in
    Array.of_list (List.map ~f:row_of_line lines)

  let height = Array.length
  let width g = Array.length g.(0)

  let positions g =
    List.concat_map
      ~f:(fun y -> List.map ~f:(fun x -> (x, y)) (List.range 0 (width g)))
      (List.range 0 (height g))
end