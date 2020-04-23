open Matrix
open Vector
open Num

module type MatAlg = sig 
  module E : Num
  module V : Vector with module E = E
  module M : Matrix with module E = E
  type elem = E.t
  type vector = V.t
  type matrix = M.t

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

module Make = functor (Elem : Num) -> struct 
  module E = Elem
  module V = Vector.Make (E)
  module M = Matrix.Make (E)

  type vector = V.t
  type matrix = M.t
  type elem = E.t

  let is_square mat = M.num_rows mat = M.num_cols mat

  let ortho = failwith "Unimplemented"

  let ortho_normal = failwith "Unimplemented"

  let row_sp = failwith "Unimplemented"

  let perp = failwith "Unimplemented"

  let change_basis = failwith "Unimplemented"

  let det = failwith "Unimplemented"

  let is_singular = failwith "Unimplemented"

  let inverse = failwith "Unimplemented"

  let factor_lu = failwith "Unimplemented"

  let factor_plu = failwith "Unimplemented"

  let eigen = failwith "Unimplemented"

  let diag = failwith "Unimplemented"

  let svd = failwith "Unimplemented"
end

