(** More complicated matrix computations.
    @author Eric Jackson [ebj29] Ralph Wang [rw484] *)

open Matrix
open Vector
open Num

(** MatAlg contains numbers [E], vectors [V] with elements of type [E.t], 
    matrices [M] with elements of type [E.t], and a variety of functions to
    manipulate them.  *)
module type MatAlg = sig 
  (** Represents matrices withwhich this module can interact. *)
  module M : Matrix

  (** Represents vectors with which this module can interact. *)
  module V = M.V

  (** Represents numbers with which this module can interact. *)
  module E = M.V.E

  (** Type of matrices. *)
  type matrix = M.t

  (** Type of vectors. *)
  type vector = M.V.t

  (** Type of numbers. *)
  type elem = M.E.t

  (** [is_square mat] is true if [mat] is a square matrix. *)
  val is_square : matrix -> bool

  (** [ortho mat] is a matrix [m] where the columns of [m] form a orthogonal
      basis for the columns of [mat]. *)
  val ortho : matrix -> matrix

  (** [ortho_normal mat] is a matrix [m] where the columns of [m] form a 
      ortho-normal basis for the columns of [mat]. *)
  val ortho_normal : matrix -> matrix

  (** [factor_qr mat] is a QR factorization of [mat]. *)
  val factor_qr : matrix -> matrix * matrix

  (** [row_sp mat] is a matrix [m] where the columns of [m] span the row space
      of [mat]. *)
  val row_sp : matrix -> matrix

  (** [perp mat] is a matrix [m] where the columns of [m] span the orthogonal
      complement of the subspace spanned by the columns of [mat]. *)
  val perp : matrix -> matrix

  (** old_basis -> new_basis -> conversion_matrix *)
  val change_basis : matrix -> matrix -> matrix

  (** [det mat] is the determinant of the matrix mat. Raises 
      DimensionMismatchException if mat is non-square *)
  val det : matrix -> elem

  (** [is_singular mat] is [true] if mat is singular and [false] otherwise. *)
  val is_singular : matrix -> bool

  (** [inverse mat] is the inverse of the matrix [mat].
      Requires: [mat] is a square matrix. *)
  val inverse : matrix -> matrix

  (** [factor_lu mat] is the LU factorization of the matrix [mat]. Returns 
      [none] if no such factorization exists. *)
  val factor_lu : matrix -> (matrix * matrix) option

  (** [factor_plu mat] is the PLU factorization of the matrix [mat]. *)
  val factor_plu : matrix -> matrix * matrix * matrix

  (** [eigen mat] is the list of eigen-values and corresponding eigen-vectors
      of [mat].
      Returns [None] if [mat] is not diagonaliziable. *)
  val eigen : matrix ->  (elem * vector) list option

  (** [diag mat] is the pair [(p,d)] where the columns of [p] are the 
      eigen-vectors of [mat] and [d] is a diagonal matrix containing the 
      eigen-values of [mat].
      Returns [None] if [mat] is not diagonizable.  *)
  val diag : matrix -> (matrix * matrix) option

  (** [basis k mat] is a matrix with k columns whose columns form an approximate
      basis for the columns of mat. Not guaranteed to be the optimal basis. If k
      is greater than the number of vectors needed to span the column space of
      mat, then this function returns a matrix with only as many vectors as
      needed to span the column space of mat. *)
  val basis : int -> matrix -> matrix
end

(** Creates a MatAlg module for matrices and vectors of type [Elem.t]. *)
module type MatAlgMaker =
  functor (Elem : Num.Num) -> MatAlg with module M = Matrix.Make(Elem)

(** Creates a MatAlg module for matrices and vectors of type [Elem.t]. *)
module Make : MatAlgMaker
