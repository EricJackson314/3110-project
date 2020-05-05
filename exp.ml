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
  let format fmt f = Format.fprintf fmt "%f" f
end

module MA = MatAlg.Make(Float)

module M = MA.M

let rec mat_to_string mat base r c =
  if r = M.num_rows mat then base
  else if c = M.num_cols mat then mat_to_string mat (base ^ "\n") (r + 1) 0
  else mat_to_string mat (base ^ (M.entry r c mat |> string_of_float) ^ " ")
    r (c + 1)

let gen_alg_test d (a : unit) =
  let mat1 = M.make d d (fun _ _ -> Random.float 10.) in
  let mat2 = M.make d d (fun _ _ -> Random.float 10.) in
  print_endline (mat_to_string mat1 "" 0 0);
  let _ = MA.is_square mat1 in
  print_endline (mat_to_string mat1 "" 0 0);
  let a = mat1 |> MA.inverse in
  print_endline (mat_to_string mat1 "" 0 0);
  let b = a |> M.mult mat1 in
  print_endline (mat_to_string mat1 "" 0 0);
  print_endline (mat_to_string b "" 0 0);
  assert (b |> M.equals (M.id d));
  assert (mat1 |> MA.inverse |> MA.det |> Float.mult (MA.det mat1)
        |> Float.equals Float.one);
  assert (M.mult mat1 mat2 |> MA.det |> Float.equals 
        (mat1 |> MA.det |> Float.mult (MA.det mat2)))

let _ = gen_alg_test 1 ()
