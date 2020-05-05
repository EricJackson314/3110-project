open Grid
open Num
open Float

module M = Matrix.Make(Num.Float)
type matrix = M.t
(* color representing the grayscale of a singel pixel, range 0. to 255.*)
type color = float
type t = color Grid.t

exception OutOfBounds

let load = failwith "Unimplemented"

let get r c i = Grid.entry r c i

let as_matrix = failwith "Unimplemented"

let width = failwith "Unimplemented"

let height = failwith "Unimplemented"

let sub = failwith "Unimplemented"

let save = failwith "Unimplemented"
