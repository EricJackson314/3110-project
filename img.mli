open MatAlg
open Matrix
open Float

(** general representation type of loaded images *)
type t

module MA : MatAlg with module M = Matrix.Make (Float)

type matrix = MA.M.t

exception OutOfBounds

(** [load s] is an [img.t] representing the image in file [s].
    Requires: [s] is a valid bitmap file in the current directory. *)
val load : string -> t

(**  [get x y img] is the grayscale pixel at coordinates x y of image img. Range
     0.- 255.
     Raises OutOfBounds if x or y is out of bounds*) 
val get : int -> int -> t -> float

val as_matrix : t -> matrix

val width : t -> int

val height : t -> int

val save : t -> string -> unit

(** [sub i x y w h] is the subsection of i with upper left corner at the 
    coordinates x y and width w, height h. Pads with white pixels if out of
    bounds. If w is negative, the function will return [sub (x + w) y (-w) h];
    negative h input is handled similarly. *)
val sub : t -> int -> int -> int -> int -> t
