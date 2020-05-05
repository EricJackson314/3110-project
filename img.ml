open Grid
open Num
open Float
open Camlimages

module M = Matrix.Make(Num.Float)
type matrix = M.t
(* color representing the grayscale of a singel pixel, range 0. to 255.*)
type color = float
type t = color Grid.t

exception OutOfBounds

let load s =
  let img = OImages.load s [] |> OImages.rgb24 in 
  let blk = (fun i j -> img#get i j |> Color.brightness |> float_of_int) in
  Grid.make_abs (img#height) (img#width) blk  

let get r c i = Grid.entry r c i

let as_matrix = failwith "Unimplemented"

let width = Grid.get_col

let height = Grid.get_row

let sub = failwith "Unimplemented"

let save = failwith "Unimplemented"
