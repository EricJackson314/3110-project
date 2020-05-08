module type Num = sig
  type t
  val add : t -> t -> t
  val add_inv : t -> t
  val mult : t -> t -> t
  val mult_inv : t -> t
  val sq_rt: t -> t
  val norm : t -> t
  val zero : t
  val one : t
  val equals : t -> t -> bool
  val compare : t -> t -> int
  val format : Format.formatter -> t -> unit
end
