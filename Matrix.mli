(* General representation type for matrices *)
type t

exception DimensionMismatchException
exception OutOfBoundsException
exception SingularMatrixException

(* [from_vector v] is the column matrix representation of vector v *)
val from_vector : Vector.t -> t

(* [num_rows t] is the number of rows in matrix t*)
val num_rows : t -> int

(* [num_cols t] is the number of columns in matrix t *)
val num_cols : t -> int

(* [is_square x] is true if x has equal numbers of rows and columns *)
val is_square : t -> bool

(* [entry row col t] is [None] if the row and col specified are out of bounds
   and [Some x] if x is the entry in t at the specified row and col. *)
val entry : int -> int -> t -> float option

(* [mult mat1 mat2] is [Some x] if x is the matrix product mat1 * mat2, or
   [None] if there is a dimension mismatch *)
val mult : t -> t -> t option

(* [add mat1 mat2] is [Some x] if x is the matrix addition mat1 + mat2, or 
   [None] if there is a dimension mismatch *)
val add : t -> t -> t option

(* [transpose mat] is [x : Matrix.t] such that entry a b mat = entry b a x for
   all a and b *)
val transpose : t -> t

(* [col_sp mat] is [x] if the columns of x form a basis for the columns space
   of mat *)
val col_sp mat : t -> t

(* [nul_sp mat] is [x] if the columns of x form a basis for the null space of
   mat *)
val nul_sp : t -> t

(* [inverse mat] is [Some x] if x * mat is the identity matrix, [None] if mat
   is singular or non-square *)
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

(* [eigen mat] is a list of eigenvalue- eigenvector list pairs, where each
   eigenvalue is paired with a list of its corresponding eigenvectors.
   Postcondition: the list contains no duplicate eigenvalues *)
val eigen : t -> (float * Vector.t list) list

(* [diag mat] is [Some mat1 * mat2] if mat is diagonalizable and the matrix
   product mat1 * mat2 * (inverse mat1) = mat, [None] otherwise *)
val diag : t -> t * t option

(* [is_herm mat] is true if mat is self-adjoint, false otherwise *)
val is_herm : t -> bool

(* [to_column_list mat] is a list containing the columns of mat as vectors, in
   order. *)
val to_column : t -> Vector.t list

(* [concat v] is [Some x] if x is a matrix whose columns are the column vector
   representation of the vectors in v or [None] if there is a dimension
   mismatch*)
val concat : Vector.t list -> t option
(* [svd mat] is [u * s * v] if the matrix product u * s * (transpose v) = mat,
   u and v are orthogonal, and s is a diagonal matrix *)
val svd : t -> t * t * t
