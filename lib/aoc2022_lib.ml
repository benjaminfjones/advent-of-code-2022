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
  val gett_exn : t -> pos:int * int -> int

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
  let get g ~col ~row = try Some g.(row).(col) with _e -> None
  let set g ~col ~row v = g.(row).(col) <- v

  let from_lines ~f lines =
    let row_of_line line = Array.of_list (List.map ~f (String.to_list line)) in
    Array.of_list (List.map ~f:row_of_line lines)

  let allocate ~height ~width ~default =
    Array.of_list (repeat ~num:height (Array.create ~len:width default))

  let height = Array.length
  let width g = Array.length g.(0)

  let valid_pos g ~col ~row =
    0 <= col && col < width g && 0 <= row && row < height g

  let positions g =
    List.concat_map
      ~f:(fun row ->
        List.map ~f:(fun col -> (col, row)) (List.range 0 (width g)))
      (List.range 0 (height g))

  let find g ~f =
    positions g |> List.find ~f:(fun (col, row) -> f (get_exn g ~col ~row))

  let cardinal_neighbors g (col, row) =
    List.filter_map
      ~f:(fun (dx, dy) ->
        let col', row' = (col + dx, row + dy) in
        if valid_pos g ~col:(col + dx) ~row:(row + dy) then Some (col', row')
        else None)
      [ (-1, 0); (0, 1); (1, 0); (0, -1) ]
end

(**
    A Pairing Heap: https://en.wikipedia.org/wiki/Pairing_heap
    Adapted from the implementation in Core_kernel
*)
module Heap : sig
  type 'a t

  val add : 'a t -> 'a -> 'a t
  val empty : compare:('a -> 'a -> int) -> 'a t
  val of_list : 'a list -> compare:('a -> 'a -> int) -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val pop_exn : 'a t -> 'a * 'a t
  val remove_top : 'a t -> 'a t option
  val remove_top_exn : 'a t -> 'a t
  val to_list : 'a t -> 'a list
  val top : 'a t -> 'a option
  val top_exn : 'a t -> 'a
end = struct
  (* `Node`s form an n-ary Pairing Tree *)
  module Node = struct
    type 'a t = { value : 'a; children : 'a t list }
  end

  open Node

  type 'a t = {
    compare : 'a -> 'a -> int;
    length : int;
    heap : 'a Node.t option;
  }

  let empty ~compare = { compare; length = 0; heap = None }

  (* Merge two Pairing Trees *)
  let merge ~compare ({ value = e1; children = nl1 } as n1)
      ({ value = e2; children = nl2 } as n2) =
    if compare e1 e2 < 0 then { value = e1; children = n2 :: nl1 }
    else { value = e2; children = n1 :: nl2 }

  (* Merge a list of Pairing Trees in pairs left-to-right, then fold back
     right to left *)
  let merge_pairs ~(compare: 'a -> 'a -> int) (trees: 'a Node.t list) =
    let rec loop acc ts =
      match ts with
      | [] -> acc
      | [ head ] -> head :: acc
      | head :: next1 :: next2 -> loop (merge ~compare head next1 :: acc) next2
    in
    match loop [] trees with
    | [] -> None
    | [ h ] -> Some h
    | x :: xs -> Some (List.fold xs ~init:x ~f:(merge ~compare))

  let add { compare; length; heap } e =
    let new_node = { value = e; children = [] } in
    let heap' =
      match heap with
      | None -> new_node
      | Some heap -> merge ~compare new_node heap
    in
    { compare; length = length + 1; heap = Some heap' }

  let top_exn t =
    match t.heap with
    | None -> failwith "top_exn called on an empty heap"
    | Some { value; _ } -> value

  let top t = try Some (top_exn t) with _ -> None

  let pop_exn { compare; length; heap } =
    match heap with
    | None -> failwith "pop_exn called on an empty heap"
    | Some { value; children } ->
        let new_heap = merge_pairs ~compare children in
        let h' = { compare; length = length - 1; heap = new_heap } in
        (value, h')

  let pop t = try Some (pop_exn t) with _ -> None

  let remove_top_exn t = snd (pop_exn t)

  let remove_top t =
    try
      let _, t' = pop_exn t in
      Some t'
    with _ -> None

  let of_list xs ~compare =
    List.fold xs ~init:(empty ~compare) ~f:(fun h x -> add h x)

  let to_list h =
    let rec aux result t =
      match pop t with None -> result | Some (x, t') -> aux (x :: result) t'
    in
    aux [] h
end
