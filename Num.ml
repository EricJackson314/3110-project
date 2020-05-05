module type Num = sig
  (**  general type of things that can go in a vector or matrix *)
  type t
  val add : t -> t -> t
  val add_inv : t -> t
  val mult : t -> t -> t
  val mult_inv : t -> t
  val sq_rt: t -> t
  (** analog of absolute value for real and complex numbers *)
  val norm : t -> t
  val one : t
  val zero : t
  val equals : t -> t -> bool
  (** [compare x y] returns 0 if x = y, -1 if x < y, 1 otherwise (optional) *)
  val compare : t -> t -> int
  val format : Format.formatter -> t -> unit
end

module Float : Num = struct
  type t = float
  let err = 0.000001
  let add a b = a +. b
  let add_inv a = -.a
  let mult a b = a *. b
  let mult_inv a = 1. /. a
  let sq_rt = Stdlib.sqrt
  let norm = Stdlib.abs_float
  let one = 1.
  let zero = 0.
  let equals a b = a -. b |> norm < err
  let compare = Stdlib.compare
  let format fmt f = Format.fprintf fmt "%f" f
end
