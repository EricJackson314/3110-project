module Complexc  = struct
  type t = float * float
  let add (a : t) (b : t) = 
    match a, b with (r1, i1), (r2, i2) -> (r1+.r2, i1+.i2)

  let add_inv (a : t) = 
    match a with (r1, i1) -> (-.r1, -.i1)

  let mult a b = 
    match a, b with (r1, i1), (r2, i2) -> 
      ((r1*.r2) -. (i1*.i2), (r1*.i2) +. (r2*.i1))

  let mult_inv a =
    match a with (r1, i1) ->
      (r1 /.(r1*.r1 +. i1*.i1), -.i1 /.(r1*.r1 +. i1*.i1))

  let norm a =
    match a with r1, i1 ->
      (sqrt((r1*.r1) +. (i1*.i1)), 0.)

  let sq_rt z : t = match z with (a, b) ->
    let theta = Stdlib.Float.atan(b /. a) in
    let lngth = match norm (a,b) with (a', _) -> (sqrt(a'), 0.) in
    mult lngth (cos(theta/. 2.), sin(theta /. 2.))

  let one : t = (1., 0.)

  let zero : t= (0., 0.)

  let compare (x : t) (y : t) = 
    match norm x, norm y with (x', _), (y',_) -> Stdlib.compare x' y'

  let equals (a : t) (b : t) : bool = 
    let err = 0.000001 in
    let dif = add a (add_inv b)|> norm in
    match dif with (r1, _) -> r1 < err

  let format fmt (f:t) = 
    match f with r1, i1 -> Format.fprintf fmt "%f + %fi" r1 i1  
end