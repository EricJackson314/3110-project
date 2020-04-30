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
  let threshold = 0.000001
  type t = float
  let add = (+.)
  let add_inv = (~-.)
  let mult = ( *. )
  let mult_inv a = 1. /. a
  let sq_rt = sqrt
  let norm = abs_float
  let one = 1.
  let zero = 0.
  let equals a b = Float.abs (a -. b) < threshold
  let compare = Stdlib.compare
  let format fmt f = Format.fprintf fmt "%f" f
end;;

module MA = MatAlg.Make (Float);;
#install_printer MA.V.format;;
#install_printer MA.M.format;;

module V = MA.V;;
module M = MA.M;;

let u = V.from_list [50.; 40.; 10.; 11.; ]
and v = V.from_list [20.; 25.; 100.; 10.; ]
and w = V.from_list [1.; 4.; 1.; 9.; 1.; ]

let a = M.concat 
    [
      V.from_list [-3.; 1.; 2.];
      V.from_list [6.; -2.; -4.];
      V.from_list [-1.; 2.; 5.];
      V.from_list [1.; 3.; 8.];
      V.from_list [-7.; -1.; -4.];
    ]