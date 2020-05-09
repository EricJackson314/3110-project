(** Image representation and related functions.
    @author Eric Jackson [ebj29] Ralph Wang [rw484] *)

open MatAlg
open Matrix
open Float

(** Type of images. *)
type t

(** Representation of the Algebra to which images can be converted. *)
module MA : MatAlg with module M = Matrix.Make (Float)

(** Type of matrices to which images can be converted. *)
type matrix = MA.M.t

(** Raised when an attempt is made to access a pixel which is out of bounds. *)
exception OutOfBounds

(** [load s] is an [img.t] representing the image in file [s].
    Requires: [s] is a valid bitmap file. *)
val load : string -> t

(**  [get x y img] is the grayscale pixel at coordinates x y of image img. Range
     0.- 255. Returns a white pixel if x or y is out of bounds *)
val get : int -> int -> t -> float

(** [as_matrix img] is the matrix representation of the image [img]. *)
val as_matrix : t -> matrix

(** [width img] is the width of the image [img]. *)
val width : t -> int

(** [height img] is the height of the image [img]. *)
val height : t -> int

(** [jang_to_grid name] takes file [name] in .jang format and converts
    it to an image. Raises FileFormatException if the file is not a .jang file,
    raises TruncatedFileException if the file does not appear to be missing
    data. *)
val jang_to_grid : string -> t

(** [save img file] saves the image [img] to the file "[file].jang". *)
val save : t -> string -> unit

(** [raw_bytes_to_file name img] saves [img] to file [name] using literal byte
    format: The first two bytes are a two byte representation of the width of
    the imaage in pixels, the rest of the bytes are pixel data.
    If [w] is the width of [img] in pixels, then the first [w] bytes will be the
    bytes of the pixels in the top row. The next [w] bytes will be the bytes of
    pixels in the second row, and so on, downwards. *)
val raw_bytes_to_file : string -> t -> unit

(** [sub i x y w h] is the subsection of i with upper left corner at the 
    coordinates x y and width w, height h. Pads with white pixels if out of
    bounds. If w is negative, the function will return [sub (x + w) y (-w) h];
    negative h input is handled similarly. *)
val sub : t -> int -> int -> int -> int -> t
