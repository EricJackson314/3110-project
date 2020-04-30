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
  val proj: t -> t -> t
  val from_list : elem list -> t
  val to_list : t -> elem list
  val make : int -> (int -> E.t) -> t
  val format : Format.formatter -> t -> unit
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
  let ( / ) = fun a b -> a * E.mult_inv b

  let dim  = List.length

  let nth = try List.nth with
    | exn -> raise OutOfBoundsException

  let add u v = List.map2 (+) u v

  let sub u v = List.map2 (-) u v

  let scale v c = List.map (( * ) c) v

  let dot u v = List.fold_left2 ( fun a u v -> a + u * v ) E.zero u v

  let norm v = List.fold_left (fun a e -> a + E.(norm e * norm e)) E.zero v |> E.sq_rt

  let normalize v =
    let n = norm v in
    if n = E.zero then v else scale v (E.mult_inv n)

  let proj u y =
    scale u (dot y u / dot u u)

  let from_list lst = lst

  let to_list v : elem list = v

  let make n f : t = List.init n f

  let format fmt v = 
    Format.fprintf fmt "[";
    List.iter (fun e -> Format.fprintf fmt "%a, " E.format e) v;
    Format.fprintf fmt "]";

end
