(** General representation of a 2D grid *)
module type GridSig = sig
  exception OutOfBounds of string

  exception IllegalDimensionsException

  type 'a t
  (* [num_rows g] is the number of rows in grid g *)
  val num_rows : 'a t -> int
  (* [num_cols g] is the number of columns in grid g *)
  val num_cols : 'a t -> int
  (* [entry r c g] is the entry at the rth row and cth column of grid g.
     Raises OutOfBounds if either r or c is negative or r >= num_rows g, or
     c >= num_cols g *)
  val entry : int -> int -> 'a t -> 'a
  (* [make r c f] is a grid with r rows and c columns, such that its entry at
     row rr and column cc is (f rr cc). This function will be re-evaluated every
     time the entry function is called on the output.
     Raises IllegalDimensionsException if either r or c is negative. *)
  val make : int -> int -> (int -> int -> 'a) -> 'a t
  (* [make_abs r c f] is a grid with r rows and c columns, such that its entry
     at row rr and column cc is (f rr cc), using the environment at the time of
     creation. Unlike [make r c f], the function f will be called only once for
     each entry.
     Raises IllegalDimensionsException if either r or c is negative *)
  val make_abs : int -> int -> (int -> int -> 'a) -> 'a t
  (* [get_row r g] is the elements of the rth row in grid g in a list,
     in order from smallest column index to the largest. 
     Raises OutOfBounds if r is out of bounds. *)
  val get_row : int -> 'a t -> 'a list
  (* [get_col c g] is the elements of the cth column in grid g, in a list,
     in order from the smallest row index to the largest
     Raises OutOfBounds if c is out of bounds. *)
  val get_col : int -> 'a t -> 'a list
end

module Grid : GridSig = struct

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
end
