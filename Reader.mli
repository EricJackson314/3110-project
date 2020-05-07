module type Reader = sig
  exception EOF
  type t
  (* begins reading the file with the given name *)
  val init : string -> t
  (* reads and returns the next byte as a signed byte, cast to int. Raises EOF
     if no more bytes are to be read *)
  val next_byte_signed : t -> int
  (* reads and returns the next byte as an unsigned byte, cast to int Raises EOF
     if no more bytes are to be read *)
  val next_byte_unsigned : t -> int
  (* whether there are more bytes to read *)
  val has_next : t -> bool
  (* [signed_to_unsigned x] is y if x is the integer cast of a signed byte and
     y is the unsigned equivalent of x. *)
  val signed_to_unsigned : int -> int
  (* this is the inverse function of signed_to_unsighed. *)
  val unsigned_to_signed : int -> int
end

module FileReader : Reader

