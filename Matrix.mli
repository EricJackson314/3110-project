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
  
  (* [is_square x] is true if x has equal numbers of rows and columns *)
  val is_square : t -> bool
  
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
  
  (* [col_sp mat] is [x] if the columns of x form a basis for the columns space
     of mat *)
  val col_sp : t -> t
  
  (* [nul_sp mat] is [x] if the columns of x form a basis for the null space of
     mat *)
  val nul_sp : t -> t
 
  (* [ortho mat] is [x] if x and mat have the same column space and x is an
     orthogonal matrix *)
  val ortho : t -> t

  (* [ortho_normal mat] is [x] if x and mat have the same column space and x
     is an orthonormal matrix *)
  val ortho_normal : t -> t

  (* [perp mat] is [x] if the columns of x form a basis for the largest possible
     subspace orthogonal to the column space of mat *)
  val perp : t -> t

  (* [change_basis mat] is [x] if changing from standard basis to the basis
     given by mat requires left-multiplying by x *)
  val change_basis : t -> t
  (* [is_sing mat] is true if mat is a square matrix and is not invertible,
     false otherwise. *)
  val is_sing : t -> bool
 
  (* [inverse mat] is [x] such that the matrix product mat * x is the identity
     matrix. Raises SingularMatrixException if mat is singular; raises 
     DimensionMismatchException if mat is non_square. *)
  val inverse : t -> t
  
  (* [factor_lu mat] is [Some (mat1*mat2)] for lower triangular matrix mat1 and 
     upper triangular matrix mat2 where (mult mat1 mat2) = mat or [None] if 
     row exchanges were required to compute lu factorization. *)
  val factor_lu : t -> t * t option
  
  (* [factor_plu mat] is p*l*u for permutation matrix p, lower triangular matrix
     l, and upper triangular matrix u such that mat is the matrix product 
     p * l * u *)
  val factor_plu : t -> t * t * t
  
  (* [ref mat] is the reduced echelon form of mat, preserving determinant if mat
     is a square matrix *)
  val ref : t -> t
  
  (* [rref mat] is the reduced row echelon form of mat *)
  val rref : t -> t
  
  (* [det mat] is the determinant of the matrix mat. Raises
     DimensionMismatchException if mat is not a square *)
  val det : t -> elem

  (* [eigen mat] is a list of eigenvalue- eigenvector list pairs, where each
     eigenvalue is paired with a list of its corresponding eigenvectors.
     Postcondition: the list contains no duplicate eigenvalues *)
  val eigen : t -> (elem * vector list) list
  
  (* [diag mat] is [Some mat1 * mat2] if mat is diagonalizable and the matrix
     product mat1 * mat2 * (inverse mat1) = mat, [None] otherwise *)
  val diag : t -> t * t option
  
  (* [is_herm mat] is true if mat is self-adjoint, false otherwise *)
  val is_herm : t -> bool
  
  (* [to_column_list mat] is a list containing the columns of mat as vectors, in
     order. *)
  val to_column : t -> vector list
  
  (* [concat v] is matrix [x] such that the nth column of x is the nth vector
     in v. Raises DimensionMismatchException if not all vectors in v are the
     same length *)
  val concat : vector list -> t
  (* [svd mat] is [u * s * v] if the matrix product u * s * (transpose v) = mat,
     u and v are orthogonal, and s is a diagonal matrix *)
  val svd : t -> t * t * t
end

module type MatrixMaker = functor (Elem : Num) -> Matrix with module E = Elem

module Make : MatrixMaker
