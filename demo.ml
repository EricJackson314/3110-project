#directory "_build";;
#load_rec "Num.cmo";;
#load_rec "Float.cmo";;
#load_rec "Complexc.cmo";;
#load_rec "Vector.cmo";;
#load_rec "Matrix.cmo";;
#load_rec "MatAlg.cmo";;
#require "camlimages";;
#require "camlimages.all_formats";;
#load_rec "Img.cmo";;

open Num;;
open Float;;
open Complexc;;
open Vector;;
open Matrix;;
open MatAlg;;

(** Real-Valued Algebra. *)
module MA = MatAlg.Make (Float);;
#install_printer MA.V.format;;
#install_printer MA.M.format;;
module V = MA.V;;
module M = MA.M;;

let u = V.from_list [50.; 40.; 10.; 11.; ]
and v = V.from_list [20.; 25.; 100.; 10.; ]
and w = V.from_list [1.; 4.; 1.; 9.; 1.; ]

let mat_5 = M.concat 
    [
      V.from_list [-3.; 1.; 2.];
      V.from_list [6.; -2.; -4.];
      V.from_list [-1.; 2.; 5.];
      V.from_list [1.; 3.; 8.];
      V.from_list [-7.; -1.; -4.];
    ]

(** Complex-Valued Algebra. *)
module MA_complex = MatAlg.Make(Complexc);;
#install_printer MA_complex.E.format;;
#install_printer MA_complex.V.format;;
#install_printer MA_complex.M.format;;
module V_complex = MA_complex.V;;
module M_complex = MA_complex.M;;

let a = V_complex.from_list [(1., 2.); (3., 5.); (7., 6.)]
and b = V_complex.from_list [(6., 4.); (2., 7.); (9., 0.)]
and c = V_complex.from_list [(3., 4.); (8., 6.); (9., 12.)]

let mat_6 = M_complex.concat [a; b; c]
let mat_7 = M_complex.transpose mat_6
