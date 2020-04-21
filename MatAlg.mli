open Matrix
open Vector
open Num

module type MatAlg = sig 
  module E : Num
  module Mat : Matrix with module E = E
  module Vec : Vector with module E = E
  type vector = Vec.t
  type matrix = Mat.t
  type elem = E.t

  val is_square : matrix -> bool
  val ortho : matrix -> matrix
  val ortho_normal : matrix -> matrix
  val row_sp : matrix -> matrix
  val perp : matrix -> matrix
  (* old_basis -> new_basis -> conversion_matrix *)
  val change_basis : matrix -> matrix -> matrix
  val det : matrix -> elem
  val is_singular : matrix -> bool
  val inverse : matrix -> matrix
  val factor_lu : matrix -> matrix * matrix option
  val factor_plu : matrix -> matrix * matrix * matrix
  val eigen : matrix -> (elem * (vector list)) list
  val diag : matrix -> matrix * matrix option
  val svd : matrix -> matrix * matrix * matrix
end

module type MatAlgMaker =
  functor (Elem : Num.Num) -> MatAlg with module E = Elem

module Make : MatAlgMaker