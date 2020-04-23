#directory "_build";;
#load_rec "Num.cmo";;
#load_rec "Vector.cmo";;
#load_rec "Matrix.cmo";;
#load_rec "MatAlg.cmo";;

open Num;;
open Vector;;
open Matrix;;
open MatAlg;;

module Float = struct
  type t = float
  let add = (+.)
  let add_inv = (~-.)
  let mult = ( *. )
  let mult_inv a = 1. /. a
  let sq_rt = sqrt
  let norm = abs_float
  let one = 1.
  let zero = 0.
  let equals a b = a = b
  let compare = Stdlib.compare
end;;

module V = Vector.Make (Float);;
module M = Matrix.Make (Float);;
module MatAlg = MatAlg.Make (Float);;

