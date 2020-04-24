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

  (** raised if an attemt is made to access a vector index that does not 
      exist. *)
  exception OutOfBoundsException

  (** [dim v] is the dimension of the vector [v]. *)
  val dim : t -> int

  (** [nth v i] is the [i]th entry in [v]. Raises OutOfBoundsException if i is
      out of bounds. *)
  val nth : t -> int -> elem

  (** [add u v] is the sum of the vectors [u] and [v]. *)
  val add : t -> t -> t

  (** [sub u v] is the difference of the vectors [u] and [v] (i.e. [u]-[v]). *)
  val sub : t ->  t -> t

  (** [scale v c] is the vector [v] multiplied by the scalar [c]. *)
  val scale : t -> elem -> t

  (** [dot u v] is the inner-product of vectors [u] and [v]. *)
  val dot : t -> t -> elem

  (** [norm v] is the length of the vector [v]. *)
  val norm : t -> elem

  (** [normalize v] is the vector of unit-length with the same 
      direction as [v]. *)
  val normalize : t -> t

  (** [from_list lst] is the vector representation of the list of elements 
      [lst]. *)
  val from_list : elem list -> t

  (** [to_list v] is a representation of the vector [v] as a list. *)
  val to_list : t -> elem list

  (** [make n f] is the [n] dimensional vector where the value of the [i]th 
      element is [f i]. *)
  val make : int -> (int -> E.t) -> t

  val format : Format.formatter -> t -> unit

end

module type VectorMaker = 
  functor (Elem : Num.Num) -> Vector with module E = Elem 

module Make : VectorMaker
