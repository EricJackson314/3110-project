open Matrix
open Vector
open Num

module type MatAlg = sig 
  module M : Matrix
  module V = M.V
  module E = M.V.E
  type matrix = M.t
  type vector = M.V.t
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

  (* old_basis -> new_basis -> conversion_matrix *)
  val change_basis : matrix -> matrix -> matrix

  (* [det mat] is the determinant of the matrix mat. Raises 
     DimensionMismatchException if mat is non-square *)
  val det : matrix -> elem
  val is_singular : matrix -> bool
  val inverse : matrix -> matrix
  val factor_lu : matrix -> (matrix * matrix) option
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

module type MatAlgMaker =
  functor (Elem : Num.Num) -> MatAlg with module M = Matrix.Make(Elem)

module Make : MatAlgMaker
