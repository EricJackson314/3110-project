open Num

module type Vector = sig
  module E : Num
  type elem = E.t
  type t
  exception OutOfBoundsException
  val dim : t -> int
  val nth : t -> int -> elem
  val add : t -> t -> t
  val sub : t ->  t -> t
  val scale : t -> elem -> t
  val dot : t -> t -> elem
  val norm : t -> elem
  val normalize : t -> t
  val from_list : elem list -> t
  val to_list : t -> elem list
  val make : int -> (int -> E.t) -> t
end

module type VectorMaker = 
  functor (Elem : Num.Num) -> Vector with module E = Elem 

module Make : VectorMaker = functor (Elem : Num) -> struct
  module E = Elem
  type elem = E.t

  type t = elem list

  exception OutOfBoundsException

  let ( + ) = E.add
  let ( - ) = fun a b -> E.add a (E.add_inv b)
  let ( * ) = E.mult

  let dim  = List.length

  let nth = try List.nth with
    | exn -> raise OutOfBoundsException

  let add u v = List.map2 (+) u v

  let sub u v = List.map2 (-) u v

  let scale v c = List.map (( * ) c) v

  let dot u v = List.fold_left2 ( fun u v a -> u*v + a) E.zero u v

  let norm v = List.fold_left (fun e a -> a + E.(norm e * norm e)) E.zero v |> E.sq_rt

  let normalize v = E.mult_inv (norm v) |> scale v

  let from_list lst = lst

  let to_list v : elem list = v

  let make n f : t = List.init n f

end
