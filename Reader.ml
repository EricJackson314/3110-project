module type Reader = sig
  exception EOF
  type t
  val init : string -> t
  val next_byte_signed : t -> int
  val next_byte_unsigned : t -> int
  val has_next : t -> bool
end

module FileReader : Reader = struct
  exception EOF
  type t = unit
  let init = failwith "Unimplemented"
  let next_byte_signed = failwith "Unimplemented"
  let next_byte_unsigned = failwith "Unimplemented"
  let has_next = failwith "Unimplemented"
end
