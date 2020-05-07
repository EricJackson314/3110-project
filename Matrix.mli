open Num
open Vector

module type Matrix = sig
  (* General representation type for matrices *)
  type t

  module E : Num
  module V : Vector with module E = E

  (* Type of vectors that this matrix module can interact with *)
  type vector = V.t

  (* Type of things stored in the matrices *)
  type elem = E.t

  exception DimensionMismatchException
  exception OutOfBoundsException
  exception SingularMatrixException

  (* [id n] is the identity matrix with n rows and n columns *)
  val id : int -> t

  (* [from_vector v] is the column matrix representation of vector v. *)
  val from_vector : vector -> t

  (* [num_rows t] is the number of rows in matrix t*)
  val num_rows : t -> int

  (* [num_cols t] is the number of columns in matrix t *)
  val num_cols : t -> int

  (* [entry row col t] is the entry at the given row and column of the matrix.
     Raises OutOfBoundsException if either row or col is out of bounds. *)
  val entry : int -> int -> t -> elem

  (* [get_row r mat] is the rth row of matrix mat as a vector *)
  val get_row : int -> t -> vector

  (* [get_col c mat] is the cth column of mat as a vector *)
  val get_col : int -> t -> vector

  (* [equals a b] is whether matrices a and b are equal to each other, that is,
     whether they hae the same number of rows and columns and whether all their
     corresponding entries are equal, under the Elem.equals function *)
  val equals : t -> t -> bool

  (* [make r c e] is [x] if x has r rows, c columns, and the entry a b x =
     e a b. Note that function e will be re-evaluated every time entry a b x
     is called, so that if random number generation or mutable state is involved
     in the function e, entry may not return the same value on each call. *)
  val make : int -> int -> (int -> int -> elem) -> t
  
  (* [make_abs r c e] is [x] if x has r rows, c columns, and entry a b x = e a b
     Unlike [make r c e], this function evaluates e only once for each entry so
     the entry function is guaranteed to return the same value each time. *)
  val make_abs : int -> int -> (int -> int -> elem) -> t

  (* [zero r c] is the zero matrix with r rows and c columns *)
  val zero : int -> int -> t

  (* [map f mat] is [x] if x has the same dimensions as mat and entry r c x =
     f (entry r c mat) *)
  val map : (elem -> elem) -> t -> t

  (* [mult mat1 mat2] is the matrix product mat1 * mat2. 
     Requires (num_cols mat1 = num_rows mat2), raises DimensionMismatchException
     otherwise *)

  val mult : t -> t -> t

  (* [add mat1 mat2] is the matrix addition mat1 + mat2. Requires mat1 and mat2
     to have the same number of rows and columns, raises
     DimensionMismatchException otherwise *)
  val add : t -> t -> t

  (* [scale scalar mat] is the scalar product scalar * mat *)
  val scale : elem -> t -> t

  (* [transpose mat] is [x : Matrix.t] such that entry a b mat = entry b a x for
     all a and b *)
  val transpose : t -> t

  (* [scale_row r scalar mat] is [x] if the entries of x are the same as mat
     in every row except row r, in which x's entries are mat's entries
     multiplied by scalar. Raises OutOfBoundsException if r is out of bounds. *)
  val scale_row : int -> elem -> t -> t

  (* [add_row r1 r2 s mat] is mat with s * the r1th row added to the r2th row.
     Raises OutOfBoundsException if r1 or r2 is out of bounds. *)
  val add_row : int -> int -> elem -> t -> t

  (* [row_swap r1 r2 mat] is mat with the r1th and r2th row swapped. Raises 
     OutOfBoundsException if r1 or r2 is out of bounds. *)
  val row_swap : int -> int -> t -> t

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

  (* [to_column mat] is a list containing the columns of mat as vectors, in
     order. *)
  val to_column : t -> vector list

  (* [concat v] is matrix [x] such that the nth column of x is the nth vector
     in v. Raises DimensionMismatchException if not all vectors in v are the
     same length *)
  val concat : vector list -> t

  (** [format fmt m] allows for pretty printing of matrices. *)
  val format : Format.formatter -> t -> unit
end

module type MatrixMaker = functor (Elem : Num) -> Matrix with module E = Elem

module Make : MatrixMaker
