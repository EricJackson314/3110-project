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
  let equals a b = Stdlib.abs_float (a -. b) < threshold
  let compare = Stdlib.compare
  let format fmt f = Format.fprintf fmt "%f" f
end

module MA = MatAlg.Make(Float)

module M = MA.M

module V = MA.V

let rec mat_to_string mat base r c =
  if r = M.num_rows mat then base
  else if c = M.num_cols mat then mat_to_string mat (base ^ "\n") (r + 1) 0
  else mat_to_string mat (base ^ (M.entry r c mat |> string_of_float) ^ " ")
    r (c + 1)

let rec ls_to_string ls =
  let rec aux = function
  | [] -> "]"
  | [a] -> (a |> string_of_int) ^ "]"
  | hd::tl -> (hd |> string_of_int) ^ "; " ^ (aux tl)
  in "[" ^ (aux ls)

let m = M.make 5 10 (fun r c -> 3 * r - 2 * c + r * c - r / (c + 1)
  |> float_of_int)

let rec fst_n n base ls =
  if n <= 0 then Some (List.rev base)
  else match ls with
  | [] -> None
  | hd::tl -> fst_n (n - 1) (hd::base) tl

let k = 3
let piv_rows = m |> M.transpose |> M.pivot_cols |> fst_n k []
let Some s = piv_rows
let mm = M.num_cols m 
let nn = M.num_rows m
let non_piv_rows = List.init nn (fun n -> n) 
  |> List.filter (fun i -> List.mem i s |> not)
let a = M.make mm k (fun r c -> M.entry (List.nth s c) r m)
let b = M.make mm (nn - k) (fun r c -> M.entry (List.nth non_piv_rows c) r m)
let ata = a |> M.mult (a |> M.transpose)
(*
let rec index base e ls = 
  match ls with 
  | [] -> None
  | hd::tl -> if hd = e then Some base else index (base + 1) e tl
*)
let _ = 
  begin match piv_rows with 
  | None -> print_endline "bad"
  | Some ls -> print_endline (ls_to_string ls)
  end;
  print_endline (mat_to_string m "" 0 0);
  print_endline (ls_to_string non_piv_rows);
  print_endline (mat_to_string a "" 0 0);
  print_endline (mat_to_string b "" 0 0);
  print_endline (mat_to_string ata "" 0 0);
  let b = MA.basis k m in
  print_endline (mat_to_string b "" 0 0)
