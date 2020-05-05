open Matrix

(* general representation type of loaded images *)
type t

module M : Matrix with module E = Num.Float
type matrix = M.t

exception OutOfBounds

val load : string -> t

(* [get x y img] is the grayscale pixel at coordinates x y of image img. Range
   0.- 255.
   Raises OutOfBounds if x or y is out of bounds*) 
val get : int -> int -> t -> float

val as_matrix : t -> matrix

val width : t -> int

val height : t -> int

val save : t -> string -> unit

(* [sub x y w h] is the subsection of t with upper left corner at the 
   coordinates x y and width w, height h. Pads with white pixels if out of
   bounds *)
val sub : t -> int -> int -> int -> int -> t







