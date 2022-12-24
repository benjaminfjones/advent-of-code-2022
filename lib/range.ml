open Core

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
