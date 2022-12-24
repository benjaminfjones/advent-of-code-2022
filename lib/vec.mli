type t

val of_ : int * int -> t
val to_ : t -> int * int
val add : t -> t -> t
val subtract : t -> t -> t
val scale : t -> int -> t
val scale_inv : t -> int -> t
val dist2 : t -> t -> int
val dist_inf : t -> t -> int
val pp : t -> string
val sexp_of_t : t -> Core.Sexp.t
val t_of_sexp : Core.Sexp.t -> t
val compare : t -> t -> int
