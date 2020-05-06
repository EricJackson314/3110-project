(* representation of a robot that writes characters into a file *)
type t = (bool ref) * string * (char list ref)

exception IllegalState

(* creates and returns a new robot that writes to file s *)
let create s = (ref false, s, ref [])

(* add char b to the list of chars to be written *)
let append (o, s, ls) b = 
  if not !o then raise IllegalState
  else ls := b::(!ls)

(* writes all chars to be written to the file *)
let close (o, s, ls) = 
  o := true; failwith "Unimplemented"
