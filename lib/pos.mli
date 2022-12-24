type t

val of_ : int * int -> t
val to_ : t -> int * int

(* Relative vector pointing from first arg (origin) to second *)
val rel : origin:t -> t -> Vec.t

(* Translate position by a vector *)
val translate : t -> Vec.t -> t

(* Join in the 2d integer lattice *)
val join : t -> t -> t

(* Meet in the 2d integer lattice *)
val meet : t -> t -> t

(* Partial ordering of the 2d integer lattice according to non-negative
   vector differences *)
val less_eq : t -> t -> bool

val pp : t -> string
val sexp_of_t : t -> Core.Sexp.t
val t_of_sexp : Core.Sexp.t -> t
val compare : t -> t -> int
