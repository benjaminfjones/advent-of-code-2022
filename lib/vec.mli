type t

val of_coords : int * int -> t
val to_coords : t -> int * int
val add : t -> t -> t
val subtract : t -> t -> t
val scale : t -> int -> t
val dist2 : t -> t -> int
val dist_inf : t -> t -> int
val pp : t -> string
