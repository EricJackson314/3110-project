(** A mathematic field with a variety of functions.
    @author Ralph Wang [rw484] Eric Jackson [ebj29] *)

(** A mathematic field. *)
module type Num = sig
  (** Type of Num. *)
  type t

  (** [add e1 e2] is [e1] plus [e2]. *)
  val add : t -> t -> t

  (** [add_inv e] is the additive inverse of [e]. *)
  val add_inv : t -> t

  (** [mult e1 e2] is [e1] times [e2]. *)
  val mult : t -> t -> t

  (** [mult_inv e] is the multiplicative inverse of [e]. *)
  val mult_inv : t -> t

  (** [sq_rt e] is the square_root of [e]. *)
  val sq_rt: t -> t

  (** [norm e] is the analog of absolute value for real and complex numbers. *)
  val norm : t -> t

  (** [zero] is the number zero. *)
  val zero : t

  (** [one] is the number one. *)
  val one : t

  (** [equals e1 e2] is the statement [e1] equals [e2]. *)
  val equals : t -> t -> bool

  (** [compare x y] is [0] if [x] equals [y], 
      [-1] if [x] is strictly less than [y],
      and  [1] otherwise. *)
  val compare : t -> t -> int

  (** [format fmt v] allows for pretty printing of numbers. *)
  val format : Format.formatter -> t -> unit

end
