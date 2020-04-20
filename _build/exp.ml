open Matrix

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
end

module Mat = Make(Float)

let id2 = Mat.make 2 2 (fun i j -> if i = j then 1. else 0.)

let uni2 = Mat.make 2 2 (fun _ _ -> 1.)

let refu2 = Mat.ref uni2

let ref2 = Mat.ref id2

let rref2 = Mat.rref id2
 
let piv = Mat.pivot_cols (Mat.id 5)

let _ = 
  List.fold_left (fun _ elem -> elem |> string_of_int |> print_endline) () piv; 
  print_endline (string_of_float (Mat.entry 0 0 refu2));
  print_endline (refu2 |> Mat.entry 0 1 |> string_of_float);
  print_endline (refu2 |> Mat.entry 1 0 |> string_of_float);
  print_endline (refu2 |> Mat.entry 1 1 |> string_of_float);

  assert (ref2 = id2);
  print_endline "Complete"

