(** Real-valued, finite-dimensional vectors with basic algebraic functions.
    @author Eric Jackson [ebj29] Ralph Wang [rw484] *)

(** Representation of a real-valued, finite-dimensional vector. *)
type t

(** [dim v] is the dimension of the vector [v]. *)
val dim : t -> int

(** [add u v] is the sum of the vectors [u] and [v]. *)
val add : t -> t -> t

(** [sub u v] is the difference of the vectors [u] and [v] (i.e. [u]-[v]). *)
val sub : t ->  t -> t

(** [scale v c] is the vector [v] multiplied by the scalar [c]. *)
val scale : t -> float -> t

(** [inner u v] is the inner-product of vectors [u] and [v]. *)
val inner : t -> t -> float

(** [norm v] is the length of the vector [v]. *)
val norm : t -> float

(** [normalize v] is the vector of unit-length with the same 
    direction as [v]. *)
val normalize : t -> t

(** [basis h] is a basis for the space spanned by [h]. *)
val basis : t list -> t list

(** [ortho_basis h] is an orthogonal basis for the space spanned by [h]. *)
val ortho_basis : t list -> t list

(** [ortho_normal_basis h] is an ortho-normal basis for the space 
    spanned by [h]. *)
val ortho_normal_basis : t list -> t list

(** [perp w] is the set of all vectors orthogonal to [w]. *)
val perp : t list -> t list

(** [change_basis b v] is the vector [v]'s coordinates relative to
    the basis [b]. *)
val change_basis : t list -> t -> t

(** [as_col v] is the column matrix containing the same entries as [v]. *)
(* val as_col : t -> Matrix.t *)