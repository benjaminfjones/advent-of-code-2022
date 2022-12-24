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
