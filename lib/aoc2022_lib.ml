open Core

module type Solver = sig
  val solve : string Array.t -> string list -> int
end

let int_sum = List.fold_left ~init:0 ~f:( + )

let int_max xs =
  match xs with
  | [] -> None
  | x0 :: rest0 ->
      let rec aux max_so_far xs =
        match xs with
        | [] -> max_so_far
        | x :: rest ->
            if x > max_so_far then aux x rest else aux max_so_far rest
      in
      Some (aux x0 rest0)
