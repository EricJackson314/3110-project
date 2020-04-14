open Num
open Vector

module type Matrix = sig
  (* General representation type for matrices *)
  type t
  
  module E : Num
  module V : Vector with module E = E

  type vector = V.t

  (* Type of things stored in the matrices *)
  type elem = E.t

  exception DimensionMismatchException
  exception OutOfBoundsException
  exception SingularMatrixException
  
  (* [from_vector v] is the column matrix representation of vector v. *)
  val from_vector : vector -> t
  
  (* [num_rows t] is the number of rows in matrix t*)
  val num_rows : t -> int
  
  (* [num_cols t] is the number of columns in matrix t *)
  val num_cols : t -> int
  
  (* [entry row col t] is the entry at the given row and column of the matrix.
     Requires row and col to be in bounds, raises OutOfBoundsException
     otherwise *)
  val entry : int -> int -> t -> elem
  
  (* [mult mat1 mat2] is the matrix product mat1 * mat2. 
     Requires (num_cols mat1 = num_rows mat2), raises DimensionMismatchException
     otherwise *)
  val mult : t -> t -> t
  
  (* [add mat1 mat2] is the matrix addition mat1 + mat2. Requires mat1 and mat2
     to have the same number of rows and columns, raises
     DimensionMismatchException otherwise *)
  val add : t -> t -> t
  
  (* [transpose mat] is [x : Matrix.t] such that entry a b mat = entry b a x for
     all a and b *)
  val transpose : t -> t
  
  (* [ref mat] is the row echelon form of mat, preserving determinant *)
  val ref : t -> t

  (* [rref mat] is the reduced row echelon form of mat *)
  val rref : t-> t

  (* [pivot_cols mat] is a list of indices of the pivot columns of mat *)
  val pivot_cols : t -> int list

  (* [col_sp mat] is [x] if the columns of x form a basis for the columns space
     of mat *)
  val col_sp : t -> t
  
  
  (* [nul_sp mat] is [x] if the columns of x form a basis for the null space of
     mat *)
  val nul_sp : t -> t
 
  (* [to_column_list mat] is a list containing the columns of mat as vectors, in
     order. *)
  val to_column : t -> vector list
  
  (* [concat v] is matrix [x] such that the nth column of x is the nth vector
     in v. Raises DimensionMismatchException if not all vectors in v are the
     same length *)
  val concat : vector list -> t
end

module type MatrixMaker = functor (Elem : Num) -> Matrix with module E = Elem

module Make : MatrixMaker
