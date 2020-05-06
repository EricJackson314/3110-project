open Grid
open Float
open Camlimages

module M = Matrix.Make(Float)
type matrix = M.t
(* color representing the grayscale of a singel pixel, range 0. to 255.*)
type color = Float.t
type t = color Grid.t

exception OutOfBounds

let load s =
  let img = OImages.load s [] |> OImages.rgb24 in 
  let blk = (fun i j -> img#get i j |> Color.brightness |> float_of_int) in
  Grid.make_abs (img#height) (img#width) blk  

let get x y img = Grid.entry x y img

let width = Grid.num_cols

let height = Grid.num_rows

let as_matrix i = M.make (height i) (width i) (fun r c -> get r c i)

let rec sub i x y w h = 
  if w < 0 then sub i (x + w) y (-w) h
  else if h < 0 then sub i x (y + h) w (-h)
  else Grid.make w h (fun r c -> 
    let xcoor = x + r in
    let ycoor = y + r in
    if xcoor < 0 || xcoor >= width i || ycoor < 0 || ycoor >= height i then 255.
    else get xcoor ycoor i)

let jang_to_grid = 
  (* use input channel to read bytes, if not the right signature then fail *)
  (* procedure: figure out width and height *)
  (* figure out number of basis vectors *)
  (* figure out basis vectors *)
  (* for each chunk, figure out the components *)
  (* combine components with basis vectors, then multiply by component scalar,
     then add the bias vector *)
  (* copy components of the vector into the grid *)
  failwith "Unimplemented"

let save = 
  (* encoding scheme: first four bytes are signed 6 5 20 20 *)
  (* next byte unsigned is the number of basis vectors *)
  (* next byte unsigned is 1/8 the width of the img in pixels, followed by 1/8 
     the height in pixels *)
  (* next series of bytes is the component scalar *)
  (* every chunk after that is components *)
  failwith "Unimplemented"





