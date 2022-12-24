open Core
module Pos = Pos
module Vec = Vec
module Heap = Heap
module Range = Range
module Grid = Grid
module Util = Util

module type Solver = sig
  val solve : string Array.t -> string list -> int
end

module Day14lib = Day14lib
