open Num

module type Vector = sig
  module E : Num
  type elem = E.t
  type t
  val dim : t -> int
  val add : t -> t -> t
  val sub : t ->  t -> t
  val scale : t -> elem -> t
  val inner : t -> t -> elem
  val norm : t -> elem
  val normalize : t -> t
  val change_basis : t list -> t -> t
end

module type VectorMaker = 
  functor (Elem : Num) -> Vector with module E = Elem 

module Make : VectorMaker = functor (Elem : Num) -> struct
  module E = Elem
  type elem = E.t

  (* TODO: Replace unit with an appropriate representation for vectors. *)
  type t = unit

  let dim v = 
    (* TODO: implement this method.  *)
    failwith "unimplemented"

  let add u v= 
    (* TODO: implement this method.  *)
    failwith "unimplemented"

  let sub u v = 
    (* TODO: implement this method.  *)
    failwith "unimplemented"

  let scale v c = 
    (* TODO: implement this method.  *)
    failwith "unimplemented"

  let inner u v = 
    (* TODO: implement this method.  *)
    failwith "unimplemented"

  let norm v = 
    (* TODO: implement this method.  *)
    failwith "unimplemented"

  let normalize v = 
    (* TODO: implement this method.  *)
    failwith "unimplemented"

  let change_basis v_list v = 
    (* TODO: implement this method.  *)
    failwith "unimplemented"
end
