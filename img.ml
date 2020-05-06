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

let save = 
  (* encoding scheme: first four bytes are 1 6 7 8 *)
  (* next byte is the number of basis vectors *)
  (* next byte is 1/8 the width of the img in pixels, followed by 1/8 the 
     height in pixels *)
  (* next 64 bytes is the bias vector *)
  (* next 64 bytes is ...*)
