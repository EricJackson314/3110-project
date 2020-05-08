exception OutOfBounds of string
exception IllegalDimensionsException

(* two different representations of grids. One as a tuple storing 
   (r, c, f) if r is number of rows, c is number of columns, and
   entry at rr cc is (f rr cc). Other as a 2D array *)
type 'a t = 
  | Fun of int * int * (int -> int -> 'a)
  | Abs of 'a array array

let num_rows m = 
  match m with
  | Fun (r, c, f) -> r
  | Abs x -> Array.length x

let num_cols m = 
  match m with
  | Fun (r, c, f) -> c
  | Abs x -> 
    if num_rows m = 0 then 0
    else Array.length x.(0)

let entry r c m =
  if r < 0 || c < 0 || r >= num_rows m || c >= num_cols m then
    raise (OutOfBounds "entry")
  else 
    match m with
    | Fun (_, _, f) -> f r c
    | Abs x -> x.(r).(c)

let make r c f = 
  if r < 0 || c < 0 then raise IllegalDimensionsException
  else Fun (r, c, f)

let make_abs r c f = 
  Abs (Array.init r (fun i -> Array.init c (fun j -> f i j)))

let get_row r m = 
  if r < 0 || r >= num_rows m then raise (OutOfBounds "get_row")
  else List.init (num_cols m) (fun i -> entry r i m)

let get_col c m = 
  if c < 0 || c >= num_cols m then raise (OutOfBounds "get_col")
  else List.init (num_rows m) (fun i -> entry i c m)