open Num

module type Matrix = sig
  type t
  module E : Num
  module V : Vector.Vector with module E = E
  type vector = V.t
  type elem = E.t

  exception DimensionMismatchException
  exception OutOfBoundsException
  exception SingularMatrixException
  
  val from_vector : vector -> t
  val num_rows : t -> int
  val num_cols : t -> int
  val is_square : t -> bool
  val entry : int -> int -> t -> float option
  val mult : t -> t -> t option
  val add : t -> t -> t option
  val transpose : t -> t
  val col_sp : t -> t
  val nul_sp : t -> t
  val inverse : t -> t
  val factor_lu : t -> t * t option
  val factor_plu : t -> t * t * t
  val ref : t -> t
  val rref : t -> t
  val eigen : t -> (float * vector list) list
  val diag : t -> t * t option
  val is_herm : t -> bool
  val to_column : t -> vector list
  val concat : vector list -> t option
  val svd : t -> t * t * t
end

module type MatrixMaker = 
  functor (Elem : Num) -> Matrix with module E = Elem

module Make : MatrixMaker = functor (Elem : Num) -> struct
  module E = Elem
  module V = Vector.Make(Elem)
  type vector = V.t
  type elem = E.t
  (* subject to change *)
  type t = elem list list

  exception DimensionMismatchException
  exception OutOfBoundsException
  exception SingularMatrixException
  
  let from_vector v = failwith "Unimplemented"
  let num_rows mat = failwith "Unimplemented"
  let num_cols mat = failwith "Unimplemented"
  let is_square mat = failwith "Unimplemented"
  let entry r c mat = failwith "Unimplemented"
  let mult mat1 mat2 = failwith "Unimplemented"
  let add mat1 mat2 = failwith "Unimplemented"
  let transpose mat = failwith "Unimplemented"
  let col_sp mat = failwith "Unimplemented" 
  let nul_sp mat = failwith "Unimplemented"
  let inverse mat = failwith "Unimplemented"
  let factor_lu mat = failwith "Unimplemented"
  let factor_plu mat = failwith "Unimplemented"
  let ref mat = failwith "Unimplemented"
  let rref mat = failwith "Unimplemented"
  let eigen mat = failwith "Unimplemented"
  let diag mat = failwith "Unimplemented"
  let is_herm mat = failwith "Unimplemented"
  let to_column mat = failwith "Unimplemented"
  let concat v_list = failwith "Unimplemented"
  let svd mat = failwith "Unimplemented"
end

