type 'a t
(** A Pairing Heap

    Adapted from the implementation in Core_kernel
    References:
      - https://github.com/janestreet/core_kernel
      - https://en.wikipedia.org/wiki/Pairing_heap
*)

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
