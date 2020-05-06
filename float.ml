
type t = float
let add a b = a +. b
let add_inv a = -.a
let mult a b = a *. b
let mult_inv a = 1. /. a
let sq_rt = Stdlib.sqrt
let norm = Stdlib.abs_float
let one = 1.
let zero = 0.
let equals a b = 
  let err = 0.000001 in
  a -. b |> norm < err
let compare (x : t) (y : t) = Stdlib.compare x y
let format fmt f = Format.fprintf fmt "%f" f

