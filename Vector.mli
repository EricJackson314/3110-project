(** Finite-dimensional vectors with basic algebraic functions.
    @author Eric Jackson [ebj29] Ralph Wang [rw484] *)

open Num

(** Representation of a finite-dimensional vector. *)
module type Vector = sig
  (** module [E] represents the vectors elements. *)
  module E : Num

  (** [elem] is the type of the vectors elements. *)
  type elem = E.t

  (** Type of Vectors. *)
  type t

  (** [dim v] is the dimension of the vector [v]. *)
  val dim : t -> int

  (** [nth v] is the nth entry in vector v *)
  val nth : t -> elem

  (** [add u v] is the sum of the vectors [u] and [v]. *)
  val add : t -> t -> t

  (** [sub u v] is the difference of the vectors [u] and [v] (i.e. [u]-[v]). *)
  val sub : t ->  t -> t

  (** [scale v c] is the vector [v] multiplied by the scalar [c]. *)
  val scale : t -> elem -> t

  (** [inner u v] is the inner-product of vectors [u] and [v]. *)
  val inner : t -> t -> elem

  (** [norm v] is the length of the vector [v]. *)
  val norm : t -> elem

  (** [normalize v] is the vector of unit-length with the same 
      direction as [v]. *)
  val normalize : t -> t

  (** [change_basis b v] is the vector [v]'s coordinates relative to
      the basis [b]. *)
  val change_basis : t list -> t -> t
end

module type VectorMaker = 
  functor (Elem : Num) -> Vector with module E = Elem 

module Make : VectorMaker
