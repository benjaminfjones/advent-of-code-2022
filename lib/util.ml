open Core

let read_lines file = In_channel.read_lines file

(* Random List enhancements *)

let sum_ints = List.sum (module Int) ~f:(fun x -> x)
let mult_ints = List.fold ~init:1 ~f:( * )
let max_ints = List.max_elt ~compare:Int.compare
let min_ints = List.min_elt ~compare:Int.compare
let or_bools = List.fold ~init:false ~f:( || )
let and_bools = List.fold ~init:true ~f:( && )
let repeat ~(num : int) elt = List.map (List.range 0 num) ~f:(fun _ -> elt)
