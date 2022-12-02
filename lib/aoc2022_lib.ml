open Core

module type Solver = sig
  val solve : string Array.t -> string list -> int
end

let sum_ints = List.sum (module Int) ~f:(fun x -> x)
let max_ints = List.max_elt ~compare:Int.compare