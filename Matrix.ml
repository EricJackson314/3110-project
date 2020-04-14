open Num
open Vector

module type Matrix = sig
  type t
  module E : Num
  module V : Vector with module E = E
  type vector = V.t
  type elem = E.t

  exception DimensionMismatchException
  exception OutOfBoundsException
  exception SingularMatrixException

  val from_vector : vector -> t
  val num_rows : t -> int
  val num_cols : t -> int
  val entry : int -> int -> t -> elem
  val mult : t -> t -> t
  val add : t -> t -> t
  val transpose : t -> t
  val ref : t -> t
  val rref : t -> t
  val pivot_cols: t -> int list
  val col_sp : t -> t
  val nul_sp : t -> t
  val to_column : t -> vector list
  val concat : vector list -> t
end

module type MatrixMaker = 
  functor (Elem : Num) -> Matrix with module E = Elem

module Make : MatrixMaker = functor (Elem : Num) -> struct
  module E = Elem
  module V = Vector.Make(Elem)
  type vector = V.t
  type elem = E.t
  (* The matrix is represented as an array of columns, and the columns are
     represented as arrays. Invariant: all inner arrays have the same length. *)
  type t = elem array array

  exception DimensionMismatchException
  exception OutOfBoundsException
  exception SingularMatrixException

  let from_vector v = 
    let len = V.dim v in
    Array.make 1 (Array.init len (fun i -> V.nth v i))
  let num_rows = Array.length
  let num_cols mat = 
    if num_rows mat = 0 then 0 else Array.length mat.(0)
  let entry r c mat = 
    if r >= 0 && r < num_rows mat && c >= 0 && c < num_cols mat then
      mat.(c).(r)
    else raise OutOfBoundsException
  (* [make rows cols ent] is matrix [x] with the specified number of rows and
     columns, such that entry r c mat = ent r c. Requires r and c to be
     non-negative. *) 
  let make rows cols ent = 
    Array.init cols (fun col -> Array.init rows (fun row -> ent row col))
  (* [copy mat] is a copy of matrix x *)
  let copy mat = 
    make (num_rows mat) (num_cols mat) (fun r c -> entry r c mat)
  let to_column mat = 
    Array.fold_right (fun arr ls -> (arr |> Array.to_list |> V.from_list)::ls)
      mat []
  let concat v_list = 
    match v_list with
    | [] -> make 0 0 (fun r c -> E.zero)
    | hd::_ -> 
      make (V.dim hd) (List.length v_list) 
        (fun r c -> V.nth (List.nth v_list c) r)
  let transpose mat = 
    make (num_cols mat) (num_rows mat) (fun r c -> entry c r mat)
  let mult mat1 mat2 = 
    if (num_cols mat1) <> (num_rows mat2) then raise DimensionMismatchException
    else 
      let tl = transpose mat1 in
      let v1 = tl |> to_column |> Array.of_list in
      let v2 = mat2 |> to_column |> Array.of_list in
      make (num_rows mat1) (num_cols mat2) (fun r c -> V.dot v1.(r) v2.(c))
  let add mat1 mat2 = 
    let rows = num_rows mat1 in
    let cols = num_cols mat1 in
    if (rows <> (num_rows mat2)) || (cols <> (num_cols mat2)) then
      raise DimensionMismatchException
    else make rows cols (fun r c -> E.add (entry r c mat1) (entry r c mat2))

  let nul_sp mat = failwith "Unimplemented"
  let ref mat = failwith "Unimplemented"
  let rref mat = failwith "Unimplemented"
  let pivot_cols = failwith "Unimplemented"
  let col_sp (mat : t) = 
    let cols = List.fold_left (fun ls i -> 
      ls@[mat.(i) |> Array.to_list |> V.from_list]) [] (pivot_cols mat) in
    concat cols

end


