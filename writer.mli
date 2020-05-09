(** Writes information to a file. *)

(** representation of a robot that writes characters into a file *)
type t

(** to be thrown when writing is attempted on a closed writer *)
exception IllegalState

(** creates and returns a new robot that writes to the given file. Note all
    pre-existing characters in the given file will be erased if the file already
    exists, and creates a new file if the file does not already exist. *)
val create : string -> t

(** [write r c] adds c to the list of characters for r to write to its file but
    does not necessarily write anything to the file. To write to the file, flush
    must be called. Note that the bytes written are unsigned, and that the
    int entered will be cast to byte. *)
val write : t -> int -> unit

(** [write_signed r c] is [write r x] where x is the unsigned equivalent of c. *)
val write_signed : t -> int -> unit

(** causes the given robot to write all of its characters to its file *)
val flush : t -> unit

(** causes the given robot to write all of its characters to its file, then
    closes the robot so that it will no longer accept more characters for 
    writing *)
val close : t -> unit
