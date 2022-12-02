module type Solver = sig
  val solve : string Array.t -> string list -> int
end
