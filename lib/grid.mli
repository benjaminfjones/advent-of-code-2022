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
