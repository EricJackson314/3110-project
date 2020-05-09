open Grid
open Float
open MatAlg
open Reader
open Writer

module MA = MatAlg.Make (Float)
module M = MA.M
module V = M.V
type matrix = M.t
type vector = M.vector
(* color representing the grayscale of a singel pixel, range 0. to 255.*)
type color = Float.t
type t = color Grid.t

exception OutOfBounds
exception FileFormatException
exception TruncatedFileException

let debug = true

let load s =
  let img = OImages.load s [] |> OImages.rgb24 in 
  let blk = (fun i j -> img#get i j |> Color.brightness |> float_of_int) in
  Grid.make_abs (img#height) (img#width) blk  

let width = Grid.num_cols

let height = Grid.num_rows

let get x y img = 
  if x < 0 || y < 0 || x >= width img || y >= height img then 255.
  else Grid.entry x y img

let as_matrix i = M.make (height i) (width i) (fun r c -> get r c i)

let rec sub i x y w h = 
  if w < 0 then sub i (x + w) y (-w) h
  else if h < 0 then sub i x (y + h) w (-h)
  else Grid.make w h (fun r c -> 
      let xcoor = x + r in
      let ycoor = y + r in
      if xcoor < 0 || xcoor >= width i || ycoor < 0 || ycoor >= height i
      then 255.
      else get xcoor ycoor i)

(* the first four bytes in a .jang file must be these bytes as signed ints *)
let jang_sig = [6; 5; 20; 20]

(* the size of little chunks images are broken into before saving *)
let block_size = 8

(* checks whether the reader given is reading from a .jang format.
   Side effect: reads and discards the first four bytes in r *)
let ensure_jang r = 
  List.init (List.length jang_sig) (fun _ -> 
      if FileReader.has_next r then FileReader.next_byte_unsigned r
      else -1)
  = jang_sig

(* Reads and returns the next byte in r as a signed byte, fails 
   if no more bytes are to be read. *)
let force_signed r = 
  if FileReader.has_next r then FileReader.next_byte_signed r
  else raise TruncatedFileException

(* Same as force_signed, but returns unsigned bytes *)
let force_unsigned r =
  if FileReader.has_next r then FileReader.next_byte_unsigned r
  else raise TruncatedFileException

(* reads n unsigned bytes from r and returns them in a list *)
let read_n n r = 
  let rec f n r base =
    if n <= 0 then base
    else f (n - 1) r ((force_unsigned r)::base)
  in
  f n r [] |> List.rev

let div_mod a b = (a / b, a mod b)

let grid_to_string g =
  let rec p r c base = 
    if r >= Grid.num_rows g then base
    else if c >= Grid.num_cols g then p (r + 1) 0 (base ^ "\n")
    else p r (c + 1) (base ^ " " ^ 
      (Grid.entry r c g |> int_of_float |> string_of_int))
  in
  p 0 0 ""

let mat_to_string mat = 
  let rec p r c base =
    if r >= M.num_rows mat then base
    else if c >= M.num_cols mat then p (r + 1) 0 (base ^ "\n")
    else p r (c + 1) 
      (base ^ (M.entry r c mat |> int_of_float |> string_of_int) ^ " ")
    in
  p 0 0 ""

(* reads bytes from src until a complete matrix is read, then returns that
   matrix *)
let read_matrix src = 
  let sc_a = force_unsigned src in
  let sc_b = force_unsigned src in
  let scalar = (sc_a |> float_of_int) /. (sc_b |> float_of_int) in
  let nr_a = force_unsigned src in
  let nr_b = force_unsigned src in
  let nc_a = force_unsigned src in
  let nc_b = force_unsigned src in
  let num_rows = 256 * nr_a + nr_b in
  let num_cols = 256 * nc_a + nc_b in
  let entry_arr = Array.init num_rows (fun i ->
    Array.init num_cols (fun j ->
      let b = force_signed src in
      (b |> float_of_int) *. scalar))
  in
  M.make_abs num_rows num_cols (fun r c -> entry_arr.(r).(c))


let basis_size = 32

(* returns a lower dimensional space for the given matrix *)
let get_basis mat = 
  let rows = block_size * block_size in
  let cols = basis_size in
  let pi = 4. *. (Stdlib.atan 1.) in
  M.make rows cols (fun r c -> 
    10. *.(Stdlib.cos ((r * c |> float_of_int) *. pi /. (rows |> float_of_int))))

let compress img = 
  (* procedure: convert each chunk to a vector and make the block_mat *)
  (* project the block_mat onto the basis and get the components *)
  (* return the basis matrix, the component matrix, the number of blocks
     across, and the number of blocks vertical *)
  let width_blocks = width img / block_size in 
  let height_blocks = height img / block_size in
  let block_pixels = block_size * block_size in
  let block_mat = M.make_abs block_pixels (width_blocks * height_blocks) 
    (fun r c -> 
      let (x, y) = div_mod c width_blocks in
      let (i, j) = div_mod r block_size in
      get (block_size * x + i) (block_size * y + j) img)
  in
  let basis = get_basis block_mat in
  let components = 
    let p = basis in
    let pt = M.transpose basis in
    let ptp_inv = M.mult pt p |> MA.inverse in
    let cp = M.mult ptp_inv pt in
    M.mult cp block_mat
  in
  (width_blocks, height_blocks, basis, components)

let decompress (w, h, basis, components) = 
  let block_mat = M.mult basis components in
  assert (M.num_cols block_mat = w * h);
  let output = 
  Grid.make (block_size * h) (block_size * w) (fun r c ->
    let (x, i) = div_mod c block_size in
    let (y, j) = div_mod r block_size in
    M.entry (block_size * j + i) (w * y + x) block_mat)
  in
  output

let read_two_byte_int_unsigned src =
  let a = force_unsigned src in
  let b = force_unsigned src in
  256 * a + b

let jang_to_grid f = 
  (* use input channel to read bytes, if not the right signature then fail *)
  (* procedure: read image width in blocks, read image height in blocks *)
  (* read the basis matrix, then read the component matrix *)
  (* then decompress *)
  let src = FileReader.init f in
  if ensure_jang src |> not then raise FileFormatException
  else 
    let width_blocks = read_two_byte_int_unsigned src in
    let height_blocks = read_two_byte_int_unsigned src in
    let basis = read_matrix src in
    let components = read_matrix src in
    decompress (width_blocks, height_blocks, basis, components)

let write_int_two_byte_unsigned writer i = 
  let to_two_byte i = 
    let (q, b) = div_mod i 256 in (q mod 256, b)
  in
  let (a, b) = to_two_byte i in
  Writer.write writer a;
  Writer.write writer b
  
(* commands writer robot w to write a byte representation of mat. *)
let write_matrix w mat = 
  (* procedure: first, scale the matrix so that all bytes fit within a signed
     byte, note the scalar *)
  (* represent the scalar as two bytes a and b, such that the scalar is 
     approximated by a / b, write these two bytes (unsigned)*)
  (* write the number of rows in the matrix as a two-byte int, write number of
     columns as a two-byte int *)
  (* record each entry as a byte *)
  let rec max r c base = 
    if r >= M.num_rows mat then base
    else if c >= M.num_cols mat then max (r + 1) 0 base
    else 
      let n = M.entry r c mat |> Float.norm in
      if n > base then max r (c + 1) n
      else max r (c + 1) base
  in
  let scalar = (max 0 1 (M.entry 0 0 mat |> Float.norm)) /. 127. in
  let to_rat f = 
    if f > 1. then 
      let a = 255 in
      let b = 255. /. f |> int_of_float in
      (a, b)
    else 
      let a = 255. *. f |> int_of_float in
      let b = 255 in
      (a, b)
  in
  let (a, b) = to_rat scalar in
  Writer.write w a;
  Writer.write w b;
  write_int_two_byte_unsigned w (M.num_rows mat);
  write_int_two_byte_unsigned w (M.num_cols mat);
  let scaled = M.scale (Float.mult_inv scalar) mat in
  let rec write_rec r c = 
    if r >= M.num_rows mat then ()
    else if c >= M.num_cols mat then write_rec (r + 1) 0
    else begin
      Writer.write_signed w (M.entry r c scaled |> int_of_float);
      write_rec r (c + 1)
    end
  in
  write_rec 0 0

let save img name = 
  (* encoding scheme: first four bytes are signed 6 5 20 20 *)
  (* next byte is width of image in blocks *)
  (* next byte is height of image in blocks *)
  (* next series of bytes is basis matrix *)
  (* next series of bytes is component matrix *)
  let writer = Writer.create (name ^ ".jang") in
  List.fold_left (fun _ b -> Writer.write writer b) () jang_sig;
  let (width_blocks, height_blocks, basis, components) = compress img in
  write_int_two_byte_unsigned writer width_blocks;
  write_int_two_byte_unsigned writer height_blocks;
  write_matrix writer basis;
  write_matrix writer components;
  Writer.close writer

let raw_bytes_to_file name img =
  let writer = Writer.create name in
  write_int_two_byte_unsigned writer (width img);
  let rec write_rec r c = 
    if r >= height img then ()
    else if c >= width img then write_rec (r + 1) 0
    else (Writer.write writer (get r c img |> int_of_float);
      write_rec r (c + 1))
    
  in 
  write_rec 0 0

let _ = if debug then
  let filename = "mat_io_test" in
  let mat = M.make_abs 7 5  (fun _ _ -> Random.float 30.) in
  let writer = Writer.create filename in
  write_matrix writer mat;
  Writer.close writer;
  let reader = FileReader.init filename in
  let cycled = read_matrix reader in
  mat |> mat_to_string |> print_endline;
  cycled |> mat_to_string |> print_endline;
  let img = load "img.bmp" in
  let filename = "img_io_test" in
  (* save img filename;
  let cycled = jang_to_grid filename in
  let err = Grid.make_abs (height img) (width img) (fun r c -> 
    (get r c img) -. (get r c cycled)) in
  err |> grid_to_string |> print_endline *)
  let byte_dump f =
    let reader = FileReader.init f in
    let rec dump r = 
      if FileReader.has_next r then 
        (FileReader.next_byte_unsigned r |> print_int;
        print_string " ";
        dump r)
    in 
    dump reader
  in
  raw_bytes_to_file filename img;
  img |> grid_to_string |> print_endline;
  byte_dump filename
