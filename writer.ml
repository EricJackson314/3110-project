type t = out_channel

exception IllegalState

let create = Stdlib.open_out

let write = Stdlib.output_char

let flush = Stdlib.flush

let close = Stdlib.close_out
