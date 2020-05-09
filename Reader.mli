(** Reads information from a file.
    @author Ralph Wang [rw484] *)

(** Reads information from a file. *)
module type Reader = sig
  (** Raised when an attempt is made to read the end of a file. *)
  exception EOF

  (** Type representing a robot which reads information from a file. *)
  type t

  (** [init file] begins reading the file [file]. *)
  val init : string -> t

  (** [next_byte_signed r] reads and returns the next byte as a signed byte, 
      cast to int. 
      Raises EOF if no more bytes are to be read *)
  val next_byte_signed : t -> int

  (** [next_byte_signed r] reads and returns the next byte as an unsigned byte, 
      cast to int.
      Raises EOF if no more bytes are to be read *)
  val next_byte_unsigned : t -> int

  (** [has_next r] is [true] if [r] has more bytes to read and [false] 
      otherwise. *)
  val has_next : t -> bool

  (** [signed_to_unsigned x] is y if x is the integer cast of a signed byte and
      y is the unsigned equivalent of x. *)
  val signed_to_unsigned : int -> int

  (** [unsigned_to_sign x] is the inverse function of [signed_to_unsighed]. *)
  val unsigned_to_signed : int -> int
end

(** A Robot that reads information from a file. *)
module FileReader : Reader

