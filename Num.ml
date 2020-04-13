module type Num = sig
  (**  general type of things that can go in a vector or matrix *)
  type t
  val add : t -> t -> t
  val add_inv : t -> t
  val mult : t -> t -> t
  val mult_inv : t -> t
  val pow: t -> t -> t
  (** analog of absolute value for real and complex numbers *)
  val norm : t -> t
  val one : t
  val zero : t
  val equals : t -> t -> bool
  (** [compare x y] returns 0 if x = y, -1 if x < y, 1 otherwise (optional) *)
  val compare : t -> t -> int
end
