open Core

module type Solver = sig
  val solve : string Array.t -> string list -> int
end

let read_lines file = In_channel.read_lines file

(* Random List enhancements *)

let sum_ints = List.sum (module Int) ~f:(fun x -> x)
let mult_ints = List.fold ~init:1 ~f:( * )
let max_ints = List.max_elt ~compare:Int.compare
let min_ints = List.min_elt ~compare:Int.compare
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
  val get_exn : t -> col:int -> row:int -> int

  (* get an element value from the grid using a position tuple *)
  val gett_exn : t -> pos:(int * int) -> int

  (* safely get a grid element *)
  val get : t -> col:int -> row:int -> int option

  (* set a grid value in-place *)
  val set : t -> col:int -> row:int -> int -> unit

  (* find a value in the grid, returing it's position *)
  val find : t -> f:(int -> bool) -> (int * int) option

  (* Construct an IntGrid from a 2d array of chars *)
  val from_lines : f:(char -> int) -> string list -> t

  (* allocate a new grid filled with a default value *)
  val allocate : height:int -> width:int -> default:int -> t

  (* size in the y-direction; always >= 1 *)
  val height : t -> int

  (* size in the x-direction; always >= 1 *)
  val width : t -> int

  (* Is the given col, row a valid grid position? *)
  val valid_pos : t -> col:int -> row:int -> bool

  (* list of all positions in the grid *)
  val positions : t -> (int * int) list

  (* list of all cardinal neighbors of a given position *)
  val cardinal_neighbors : t -> int * int -> (int * int) list
end = struct
  type t = int Array.t Array.t

  let get_exn g ~col ~row = g.(row).(col)
  let gett_exn g ~pos = g.(snd pos).(fst pos)

  let get g ~col ~row =
      try Some g.(row).(col) with
      | _e -> None

  let set g ~col ~row v =
    g.(row).(col) <- v

  let from_lines ~f lines =
    let row_of_line line =
      Array.of_list
        (List.map ~f (String.to_list line))
    in
    Array.of_list (List.map ~f:row_of_line lines)

  let allocate ~height ~width ~default =
    Array.of_list (repeat ~num:height (Array.create ~len:width default))

  let height = Array.length
  let width g = Array.length g.(0)
  let valid_pos g ~col ~row =
    0 <= col && col < width g && 0 <= row && row < height g

  let positions g =
    List.concat_map
      ~f:(fun row -> List.map ~f:(fun col -> (col, row)) (List.range 0 (width g)))
      (List.range 0 (height g))

  let find g ~f =
    positions g
      |> List.find ~f:(fun (col, row) -> f (get_exn g ~col:col ~row:row))


  let cardinal_neighbors g (col, row) =
    List.filter_map
      ~f:(fun (dx, dy) ->
          let col', row' = col + dx, row + dy in
          if valid_pos g ~col:(col + dx) ~row:(row + dy) then Some (col', row') else
              None)
      [(-1, 0); (0, 1); (1, 0); (0, -1)]
end
