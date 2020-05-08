open Reader.FileReader

type t = out_channel

exception IllegalState

let create = Stdlib.open_out

let write w ch = Stdlib.output_char w (char_of_int ch)

let write_signed w ch = write w (signed_to_unsigned ch)

let flush = Stdlib.flush

let close = Stdlib.close_out
