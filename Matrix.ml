open Num
open Vector

module type Matrix = sig
  type t
  module E : Num
  module V : Vector with module E = E
  type vector = V.t
  type elem = E.t

  exception DimensionMismatchException
  exception OutOfBoundsException
  exception SingularMatrixException

  val id : int -> t
  val from_vector : vector -> t
  val num_rows : t -> int
  val num_cols : t -> int
  val entry : int -> int -> t -> elem
  val make : int -> int -> (int -> int -> elem) -> t
  val map : (elem -> elem) -> t -> t
  val mult : t -> t -> t
  val add : t -> t -> t
  val transpose : t -> t
  val scale_row : int -> elem -> t -> t
  val add_row : int -> int -> elem -> t -> t
  val row_swap : int -> int -> t -> t
  val ref : t -> t
  val rref : t -> t
  val pivot_cols: t -> int list
  val col_sp : t -> t
  val nul_sp : t -> t
  val to_column : t -> vector list
  val concat : vector list -> t
  val format : Format.formatter -> t -> unit
end

module type MatrixMaker = 
  functor (Elem : Num) -> Matrix with module E = Elem

module Make : MatrixMaker = functor (Elem : Num) -> struct
  module E = Elem
  module V = Vector.Make(Elem)
  type vector = V.t
  type elem = E.t
  (* The matrix is represented as an array of columns, and the columns are
     represented as arrays. Invariant: all inner arrays have the same length. *)
  (* The elements of any such array must not be modified outside any public 
     function it's created in. *)
  type t = elem array array

  exception DimensionMismatchException
  exception OutOfBoundsException
  exception SingularMatrixException

  let from_vector v = 
    let len = V.dim v in
    Array.make 1 (Array.init len (fun i -> V.nth v i))

  let num_cols = Array.length

  let num_rows mat = 
    if num_cols mat = 0 then 0 else Array.length mat.(0)

  let entry r c mat = 
    if r >= 0 && r < num_rows mat && c >= 0 && c < num_cols mat then
      mat.(c).(r)
    else raise OutOfBoundsException

  let make rows cols ent = 
    Array.init cols (fun col -> Array.init rows (fun row -> ent row col))

  let map f mat =
    make (num_rows mat) (num_cols mat) (fun r c -> f (entry r c mat))

  (* [copy mat] is a copy of matrix x *)
  let copy mat = 
    make (num_rows mat) (num_cols mat) (fun r c -> entry r c mat)

  let id n = make n n (fun r c -> if r = c then E.one else E.zero)

  let to_column mat = 
    Array.fold_right (fun arr ls -> (arr |> Array.to_list |> V.from_list)::ls)
      mat []

  let concat v_list = 
    match v_list with
    | [] -> make 0 0 (fun r c -> E.zero)
    | hd::_ -> 
      make (V.dim hd) (List.length v_list) 
        (fun r c -> V.nth (List.nth v_list c) r)

  let transpose mat = 
    make (num_cols mat) (num_rows mat) (fun r c -> entry c r mat)

  let mult mat1 mat2 = 
    if (num_cols mat1) <> (num_rows mat2) then raise DimensionMismatchException
    else 
      let tl = transpose mat1 in
      let v1 = tl |> to_column |> Array.of_list in
      let v2 = mat2 |> to_column |> Array.of_list in
      make (num_rows mat1) (num_cols mat2) (fun r c -> V.dot v1.(r) v2.(c))

  let add mat1 mat2 = 
    let rows = num_rows mat1 in
    let cols = num_cols mat1 in
    if (rows <> (num_rows mat2)) || (cols <> (num_cols mat2)) then
      raise DimensionMismatchException
    else make rows cols (fun r c -> E.add (entry r c mat1) (entry r c mat2))

  let scale_row r scalar mat =
    let rows = num_rows mat in
    let cols = num_cols mat in 
    if r < 0 || r > num_rows mat then raise OutOfBoundsException
    else make rows cols 
        (fun row col -> entry row col mat 
                        |> fun v -> if r = row then E.mult scalar v else v)

  let add_row r1 r2 s mat = 
    let rows = num_rows mat in
    let cols = num_cols mat in
    if r1 < 0 || r1 > rows || r2 < 0 || r2 > rows 
    then raise OutOfBoundsException
    else make rows cols
        (fun row col -> entry row col mat
                        |> fun v -> if r2 = row then E.add v (E.mult s (entry r1 col mat))
                        else v) 

  let row_swap r1 r2 mat =
    let rows = num_rows mat in
    let cols = num_cols mat in
    if r1 < 0 || r1 > rows || r2 < 0 || r2 > rows 
    then raise OutOfBoundsException
    else make rows cols
        (fun row col ->
           if row = r1 then entry r2 col mat else
           if row = r2 then entry r1 col mat else
             entry row col mat)

  (* [eliminate r c mat] is x if elimination is performed on mat on the part of
     the matrix below and to the right of row r and column c, inclusive. Assumes
     row is between 0 and num_rows mat inclusive and col is between 0 and
     num_cols mat inclusive, raises OutOfBoundsException otherwise. *)
  let rec eliminate row col mat =
    (* general sketch of procedure: *)
    (* if col = num_cols or row = num_rows return mat *) 
    (* find the first pivot row and swap with row r *)
    (* if no such row, call eliminate again on the next column, same row*)
    (* eliminate the rest of the rows *)
    (* then call eliminate on the next row and column *)
    if (col = num_cols mat) || (row = num_rows mat) then mat
    else
      let rows = num_rows mat in
      let rec find_piv r = 
        if r = rows then None 
        else if entry r col mat |> E.equals E.zero then find_piv (r + 1)
        else Some r
      in
      match find_piv row with
      | None -> eliminate row (col + 1) mat
      | Some r -> 
        let m = row_swap row r mat in
        let c = E.mult_inv (entry row col m) in
        let coeff rr = E.add_inv (E.mult (entry rr col m) c) in
        let elim_row rrr = add_row row rrr (coeff rrr) in
        let rec elim r x = 
          if r = rows then x
          else elim (r + 1) (elim_row r x)
        in
        eliminate (row + 1) (col + 1) (elim (row + 1) m)
  let ref = eliminate 0 0

  let row_op = function
    | h::t -> 
      h::(List.fold_left (fun acc v -> V.(sub v (scale h (nth v 0)))::acc) [] t)
    | [] -> []

  let ref' mat =
    mat 
    |> transpose
    |> to_column
    |> row_op
    |> concat
    |> transpose

  (* let ref = ref' *)

  (* [collect_pivots r c ls mat] is the list of the columns of the pivot
     positions below and to the right of row r and column c, inclusive pushed on
     to ls. Assumes that the part of the matrix searched is in row echelon 
     form *)
  let rec collect_pivots r c ls mat = 
    if r = num_rows mat || c = num_cols mat then ls
    else if entry r c mat |> E.equals E.zero then collect_pivots r (c + 1) ls mat
    else collect_pivots (r + 1) (c + 1) (c::ls) mat

  let pivot_cols mat =
    let red = ref mat in
    List.rev (collect_pivots 0 0 [] red)

  (* procedure sketch: find all the pivot columns, scale those columns then
     do up elimination. *)
  let rref mat = 
    let scale_up_elim col x = 
      let rec get_row row = 
        if entry row col x |> E.equals E.zero then get_row (row - 1)
        else row
      in
      let piv_r = get_row ((num_rows x) - 1) in
      let m = scale_row piv_r (E.mult_inv (entry piv_r col x)) x in
      let rec up_elim r y = 
        if r < 0 then y
        else
          up_elim (r - 1) (add_row piv_r r (entry r col y |> E.add_inv) y)
      in up_elim (piv_r - 1) m
    in
    List.fold_right 
      (fun piv_col x -> scale_up_elim piv_col x) (pivot_cols mat) (ref mat)

  (** [insert i e] is the list [lst] with element [e] inserted into the [i]th
      index. *)
  let rec insert i e lst = if i = 0 then e::lst else match lst with
      | [] -> raise OutOfBoundsException
      | h::t -> h::insert (i-1) e t

  (** [non_pivot mat] is the list of indices of non-pivot columns in [mat]. *)
  let non_pivot mat =
    List.init (num_rows mat) (fun i -> i) |>
    List.filter (fun x -> List.mem x (pivot_cols mat))

  (** [idx_vecs idxs vecs] is the list of vectors composed by the vectors in the 
      indices of [idxs].
      Requires: [idxs] is sorted least to greatest.
  *)
  let idx_vecs idxs vecs =
    let rec vecs_idx acc idxs vecs = match idxs with
      | [] -> acc
      | h::t -> vecs_idx ((List.nth vecs h)::acc) t vecs
    in List.rev (vecs_idx [] idxs vecs)

  (** [insert vecs idxs] inserts the standard basis vector e_i into the ith 
      element of vecs for each i in idxs. *)
  let rec insert_vecs idxs vecs =
    match idxs with
    | [] -> vecs
    | h::t ->
      insert_vecs t (insert h (V.make (V.dim (List.hd vecs))
                                 (fun i -> if i = h then V.E.(add_inv one) else V.E.zero)) vecs)

  (** [fst_n n lst] is the first n elements of lst. Raises OutOfBoundsException
      if [n > List.length lst]. *)
  let fst_n n lst = 
    let rec fst acc n lst = match n, lst with 
      | 0, _ -> acc
      | n, h::t -> fst (h::acc) (n-1) t
      | _, [] -> raise OutOfBoundsException
    in List.rev (fst [] n lst)

  let nul_sp mat =
    let cols = num_cols mat in
    let non_pivot_cols = non_pivot mat in
    mat 
    |> rref
    |> transpose
    |> to_column
    |> insert_vecs non_pivot_cols
    |> fst_n cols
    |> concat
    |> mult
      (make cols cols 
         (fun i j -> if i = j then V.E.(add_inv one) else V.E.zero))
    |> add (id cols) |> add (id cols)
    |> transpose
    |> to_column
    |> idx_vecs non_pivot_cols
    |> concat

  let col_sp (mat : t) = 
    let cols = List.fold_left (fun ls i -> 
        ls@[mat.(i) |> Array.to_list |> V.from_list]) [] (pivot_cols mat) in
    concat cols

  let format fmt m =
    Format.fprintf fmt "\n";
    m 
    |> transpose 
    |> to_column 
    |> List.iter (fun v -> Format.fprintf fmt "%a\n" V.format v);
end

