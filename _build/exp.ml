open Matrix

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
  let format fmt x = ()
end

module A = MatAlg.Make(Float)

module Mat = A.M

let mat = Mat.concat 
           [
             Mat.V.from_list [-3.; 1.; 2.];
             Mat.V.from_list [6.; -2.; -4.];
             Mat.V.from_list [-1.; 2.; 5.];
             Mat.V.from_list [1.; 3.; 8.];
             Mat.V.from_list [-7.; -1.; -4.];
           ]

let mat2 = Mat.concat 
           [
             Mat.V.from_list [1.; 0.; 0.];
             Mat.V.from_list [-2.; 0.; 0.];
             Mat.V.from_list [0.; 1.; 0.];
             Mat.V.from_list [-1.; 2.; 0.];
             Mat.V.from_list [3.; -2.; 0.];
           ]

let matref = Mat.ref mat

let matrref = Mat.rref mat

let piv = Mat.pivot_cols mat

let rpiv = Mat.pivot_cols matrref

let mat22 = Mat.make 2 2 (fun _ _ -> 1.)

let matr2 = Mat.make 2 2 (fun r c -> if r = 0 then 1. else 0.)

let rec list_to_string ls base = 
  match ls with
  | [] -> base
  | hd::tl -> list_to_string tl (base ^ (string_of_int hd) ^ " ")

let rec mat_to_string mat base r c =
  if r = Mat.num_rows mat then base
  else if c = Mat.num_cols mat then mat_to_string mat (base ^ "\n") (r + 1) 0
  else mat_to_string mat (base ^ (Mat.entry r c mat |> string_of_float) ^ " ")
    r (c + 1)

let mat = Mat.zero 7 5

let (p, l, u) = A.factor_plu mat

let _ = 
  assert (Mat.equals matrref mat2);
  assert (Mat.equals (Mat.ref mat22) matr2);
  print_endline (mat_to_string p "" 0 0);
  print_endline (mat_to_string l "" 0 0);
  print_endline (mat_to_string u "" 0 0);
  print_endline (mat_to_string mat "" 0 0);
  print_endline (mat_to_string matref "" 0 0);
  print_endline (mat_to_string matrref "" 0 0);
  print_endline (mat_to_string mat2 "" 0 0);
  print_endline (list_to_string rpiv "");
  print_endline (string_of_float (Mat.entry 1 1 matrref));
  print_endline "Complete"

