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
  val eigen : matrix -> (elem * (vector list)) list
  val diag : matrix -> (matrix * matrix) option
  val svd : matrix -> matrix * matrix * matrix
end

module type MatAlgMaker =
  functor (Elem : Num.Num) -> MatAlg with module M = Matrix.Make(Elem)

module Make : MatAlgMaker
