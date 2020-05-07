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
end

module FileReader : Reader
