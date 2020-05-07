open Grid
open Float
open Camlimages
open Reader

module M = Matrix.Make(Float)
module V = M.V
type matrix = M.t
type vector = M.vector
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

(* the first four bytes in a .jang file must be these bytes as signed ints *)
let jang_sig = [6; 5; 20; 20]

(* the size of little chunks images are broken into before saving *)
let block_size = 8

(* returns the next byte in r as a signed byte option, and causes r to move 
   past that byte. If no more bytes, then None *)
let safe_signed r = 
  if FileReader.has_next r then Some (FileReader.next_byte_signed r)
  else None

(* same as safe_signed, but returns unsigned bytes *)
let safe_unsigned r = 
  if FileReader.has_next r then Some (FileReader.next_byte_unsigned r)
  else None

(* checks whether the reader given is reading from a .jang format.
   Side effect: reads and discards the first four bytes in r *)
let ensure_jang r = 
  List.init (List.length jang_sig) (fun _ -> 
    match safe_unsigned r with
    | Some x -> x
    | None -> -1)
  = jang_sig

(* Reads and returns the next byte in r as a signed byte, fails 
   if no more bytes are to be read. *)
let force_signed r = 
  match safe_signed r with
  | Some x -> x
  | None -> failwith "File too short"

(* Same as force_signed, but returns unsigned bytes *)
let force_unsigned r =
  match safe_unsigned r with
  | Some x -> x
  | None -> failwith "File too short"

let read_scalar r : int = failwith "Unimplemented"

(* [collect_vectors count dims src base] reads the first count vectors of
   dimension dims from src and returns them in an array, in order. The
   vectors store signed bytes from src and cast them to float. *)
let collect_vectors count dims src = failwith "Unimplemented" 

let jang_to_grid f = 
  (* use input channel to read bytes, if not the right signature then fail *)
  (* procedure: figure out width and height *)
  (* figure out number of basis vectors *)
  (* figure out basis vectors *)
  (* for each chunk, figure out the components *)
  (* combine components with basis vectors, then multiply by component scalar,
     then add the bias vector *)
  (* copy components of the vector into the grid *)
  let r = FileReader.init f in
  if r |> ensure_jang |> not then failwith "Invalid format"
  else 
    let size_basis = force_unsigned r in
    let width_blocks = force_unsigned r in
    let height_blocks = force_unsigned r in
    let scalar = read_scalar r in
    let pixels_per_block = block_size * block_size in
    let basis_vectors = collect_vectors size_basis pixels_per_block r in
    let basis_mat = 
      M.make pixels_per_block size_basis
        (fun r c -> V.nth basis_vectors.(c) r)
    in
    let component_arr = 
      Array.init height_blocks (fun i -> 
        Array.init width_blocks (fun j -> 
          collect_vectors 1 size_basis r))
    in
    let block_arr = Array.map (fun c_arr -> Array.map (fun comp -> 
      let comp_mat = comp |> M.from_vector in
      M.mult basis_mat comp_mat |> M.scale (float_of_int scalar)) c_arr)
      component_arr
    in
    let div_mod a b = (a / b, a mod b) in
    M.make (pixels_per_block * height_blocks) (pixels_per_block * width_blocks)
      (fun r c ->
        let (rr, i) = div_mod r pixels_per_block in
        let (cc, j) = div_mod c pixels_per_block in
        M.entry i j (block_arr.(rr).(cc)))

let save = 
  (* encoding scheme: first four bytes are signed 6 5 20 20 *)
  (* next byte unsigned is the number of basis vectors *)
  (* next byte unsigned is 1/8 the width of the img in pixels, followed by 1/8 
     the height in pixels *)
  (* next series of bytes is the component scalar *)
  (* next chunk of 64 signed bytes is the first basis vector *)
  (* following chunks of 64 signed bytes are the rest of the basis vectors *)
  (* every chunk after that is components *)
  (* components chunks are ordered first by row, then by column. For example,
     if (i, j) represented the (row, column) of a chunk, then the order would
     be (0, 0), (0, 1), (0, 2), ... (1, 0), (1, 1), .. and so on *)
  failwith "Unimplemented"





