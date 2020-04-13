open Num

module type Vector = sig
  module E : Num
  type elem = E.t
  type t
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
end

module type VectorMaker = 
  functor (Elem : Num) -> Vector with module E = Elem 

module Make : VectorMaker = functor (Elem : Num) -> struct
  module E = Elem
  type elem = E.t

  type t = elem list

  let ( + ) = E.add
  let ( - ) = fun a b -> E.add a (E.add_inv b)
  let ( * ) = E.mult

  let dim  = List.length

  let nth = List.nth

  let add u v = List.map2 (+) u v

  let sub u v = List.map2 (-) u v

  let scale v c = List.map (( * ) c) v

  let dot u v = List.fold_left2 ( fun u v a -> u*v + a) E.zero u v

  let norm v =
    let half = E.(mult_inv (one + one)) in
    let sum_sq = List.fold_left (fun e a -> a + E.(norm e * norm e)) E.zero v in
    E.pow sum_sq half

  let normalize v = E.mult_inv (norm v) |> scale v

  let from_list lst = lst

  let to_list v : elem list = v 

end
