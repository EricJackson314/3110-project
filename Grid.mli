(** General representation of a 2D grid. *)

(** Raised if an attempt is made to access a grid index that is out of 
    bounds. *)
exception OutOfBounds of string

(** Raised is the dimensions of the grid are illigal (i.e. negative). *)
exception IllegalDimensionsException

(** Type of the grid. *)
type 'a t

(** [num_rows g] is the number of rows in grid g *)
val num_rows : 'a t -> int

(** [num_cols g] is the number of columns in grid g *)
val num_cols : 'a t -> int

(**  [entry r c g] is the entry at the rth row and cth column of grid g.
     Raises [OutOfBounds] if either r or c is negative or r >= num_rows g, or
     c >= num_cols g *)
val entry : int -> int -> 'a t -> 'a

(** [make r c f] is a grid with r rows and c columns, such that its entry at
    row rr and column cc is (f rr cc). This function will be re-evaluated every
    time the entry function is called on the output.
    Raises [IllegalDimensionsException] if either r or c is negative. *)
val make : int -> int -> (int -> int -> 'a) -> 'a t

(** [make_abs r c f] is a grid with r rows and c columns, such that its entry
    at row rr and column cc is (f rr cc), using the environment at the time of
    creation. Unlike [make r c f], the function f will be called only once for
    each entry.
    Raises [IllegalDimensionsException] if either r or c is negative *)
val make_abs : int -> int -> (int -> int -> 'a) -> 'a t

(** [get_row r g] is the elements of the rth row in grid g in a list,
    in order from smallest column index to the largest. 
    Raises [OutOfBounds] if r is out of bounds. *)
val get_row : int -> 'a t -> 'a list

(** [get_col c g] is the elements of the cth column in grid g, in a list,
    in order from the smallest row index to the largest
    Raises [OutOfBounds] if c is out of bounds. *)
val get_col : int -> 'a t -> 'a list
