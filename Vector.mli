(** Finite-dimensional vectors with basic algebraic functions.
    @author Eric Jackson *)

(** Representation of a finite-dimensional vector. *)
type 'a t

(** [dim v] is the dimension of the vector [v]. *)
val dim : 'a t -> int

(** [add u v] is the sum of the vectors [u] and [v]. *)
val add : 'a t -> 'a t -> 'a t

(** [sub u v] is the difference of the vectors [u] and [v] (i.e. [u]-[v]). *)
val sub : 'a t -> 'a t -> 'a t

(** [scale v c] is the vector [v] multiplied by the scalar [c]. *)
val scale : 'a t -> 'a -> 'a t

(** [inner u v] is the inner-product of vectors [u] and [v]. *)
val inner : 'a t -> 'a t -> 'a 

(** [norm v] is the length of the vector [v]. *)
val norm : 'a t -> 'a

(** [normalize v] is the vector of unit-length with the same 
    direction as [v]. *)
val normalize : 'a t -> 'a t

(** [basis h] is a basis for the space spanned by [h]. *)
val basis : 'a t list -> 'a t list

(** [ortho_basis h] is an orthogonal basis for the space spanned by [h]. *)
val ortho_basis : 'a t list -> 'a t list

(** [ortho_normal_basis h] is an ortho-normal basis for the space 
    spanned by [h]. *)
val ortho_normal_basis : 'a t list -> 'a t list

(** [perp w] is the set of all vectors orthogonal to [w]. *)
val perp : 'a t list -> 'a t list

(** [change_basis b v] is the vector [v]'s coordinates relative to
    the basis [b]. *)
val change_basis : 'a t list -> 'a t -> 'a t