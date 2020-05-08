module type Reader = sig
  exception EOF
  type t
  val init : string -> t
  val next_byte_signed : t -> int
  val next_byte_unsigned : t -> int
  val has_next : t -> bool
  val signed_to_unsigned : int -> int
  val unsigned_to_signed : int -> int
end

module FileReader : Reader = struct
  exception EOF
  (* Representation of a Reader. The bytes come from the in_channel. The int
     represents the next byte to be read or -1 if no more bytes to read. *)
  type t = int ref * Stdlib.in_channel
  (* attempts to read a char from chan, returns the unsigned int representation
     of the char, or -1 if end of file is reached *)
  let safe_read chan = 
    try 
      int_of_char (Stdlib.input_char chan)
    with Stdlib.End_of_file -> -1
  (* pulls the next byte from chan into the buffer x. Postcondition: 
     x is the next byte to be read from this reader, or is -1 if no more bytes
     are to be read *)
  let pull (x, chan) =
    if !x = -1 then x := safe_read chan
    else ()
  let init f = 
    let chan = Stdlib.open_in f in
    (ref (-1), chan)
  let next_byte_unsigned (x, chan) = 
    pull (x, chan);
    if !x = -1 then raise EOF
    else
    let output = !x in
    x := safe_read chan;
    output
  let signed_to_unsigned x = if x >= 0 then x else x + 256
  let unsigned_to_signed x = if x < 128 then x else x - 256
  let next_byte_signed c = 
    next_byte_unsigned c 
    |> unsigned_to_signed
  let has_next (x, chan) = 
    pull (x, chan); !x <> -1
end
