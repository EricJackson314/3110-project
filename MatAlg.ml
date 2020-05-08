open Matrix
open Vector
open Num

module type MatAlg = sig 
  module M : Matrix
  module V = M.V
  module E = M.V.E
  type matrix = M.t
  type vector = M.V.t
  type elem = M.V.E.t

  val is_square : matrix -> bool
  val ortho : matrix -> matrix
  val ortho_normal : matrix -> matrix
  val factor_qr : matrix -> matrix * matrix
  val row_sp : matrix -> matrix
  val perp : matrix -> matrix
  (* old_basis -> new_basis -> conversion_matrix *)
  val change_basis : matrix -> matrix -> matrix
  val det : matrix -> elem
  val is_singular : matrix -> bool
  val inverse : matrix -> matrix
  val factor_lu : matrix -> (matrix * matrix) option
  val factor_plu : matrix -> matrix * matrix * matrix
  val eigen : matrix -> (elem * vector) list option
  val diag : matrix -> (matrix * matrix) option
  val basis : int -> matrix -> matrix
end

module type MatAlgMaker =
  functor (Elem : Num.Num) -> MatAlg with module M = Matrix.Make(Elem)

module Make = functor (Elem : Num) -> struct
  module M = Matrix.Make (Elem)
  module V = M.V
  module E = M.V.E

  type vector = V.t
  type matrix = M.t
  type elem = E.t

  let to_abs mat =
    let r = M.num_rows mat in
    let c = M.num_cols mat in
    M.make_abs r c (fun r c -> M.entry r c mat)

  let is_square mat = M.num_rows mat = M.num_cols mat

  (** [gram_schmidt v x] is the vector [x'] which is the component of [x]
      orthogonal to the vectors in [v]. *)
  let gram_schmidt v x = 
    if v = [] then x 
    else List.fold_left (fun x' v' -> V.sub x' (V.proj v' x')) x v

  (** [ortho mat] applies the Gram-Schmidt process to the column 
      vectors of [mat]. *)
  let ortho mat = 
    mat
    |> M.col_sp
    |> M.to_column
    |> List.fold_left (fun v x -> (gram_schmidt v x)::v) []
    |> List.rev
    |> M.concat

  let ortho_normal mat =
    mat
    |> ortho
    |> M.to_column
    |> List.map M.V.normalize
    |> M.concat

  let factor_qr mat =
    let q = ortho_normal mat in
    let r = M.(mult (transpose q) mat) in
    q, r

  let row_sp mat = 
    mat 
    |> M.transpose 
    |> M.col_sp

  let perp mat =
    mat
    |> M.transpose
    |> M.nul_sp 

  let rec diag_fold f i base mat = 
    if i >= M.num_rows mat then base
    else diag_fold f (i + 1) (f (M.entry i i mat) base) mat 

  let det x =
    if not (is_square x) then raise M.DimensionMismatchException
    else 
      let m = M.ref x in
      diag_fold (fun e b -> E.mult e b) 0 E.one m

  let is_singular x = det x |> E.equals E.zero

  let inverse mat = 
    if mat |> is_square |> not then raise M.DimensionMismatchException
    else if mat |> is_singular then raise M.SingularMatrixException
    else 
      let dim = M.num_rows mat in
      let gjm = M.make dim (2 * dim)
          (fun r c -> if c < dim then M.entry r c mat 
            else M.entry r (c - dim) (M.id dim))
      in
      let x = M.rref gjm in
      M.make dim dim (fun r c -> M.entry r (c + dim) x)

  let factor_plu a = 
    let d = M.num_rows a in
    let c = M.num_cols a in
    let g = M.make d (c + d) (fun r col -> 
        if col < c then M.entry r col a
        else if (col - c) = r then E.one
        else E.zero)
    in
    let eg = M.ref g in
    let u = M.make d c (fun r c -> M.entry r c eg) in
    let e = M.make d d (fun r col -> M.entry r (c + col) eg) in
    let ei = inverse e in
    (* column of the last non-zero entry in row r of matrix mat *)
    let rec n r mat base col = 
      if col = d then base
      else if M.entry r col mat |> E.equals E.zero then n r mat base (col + 1)
      else n r mat col (col + 1)
    in
    (* generates a list of int-int pairs, where the first int is the row of the
       matrix mat and the second int is (n 0 mat -1 0) *)
    let rec map mat r base =
      if r = M.num_rows mat then base
      else map mat (r + 1) ((r, n r mat (-1) 0)::base)
    in
    let m = map ei 0 [] in
    let rec findv k ls = 
      match ls with
      | [] -> failwith "BAD!"
      | (kk, vv)::tl -> if k = kk then vv else findv k tl
    in
    let rec findk v ls = 
      match ls with
      | [] -> failwith "BAD!"
      | (kk, vv):: tl -> if v = vv then kk else findk v tl
    in
    let p = M.make d d (fun r c -> if c = findv r m then E.one else E.zero) in
    let l = M.make d d (fun r c -> M.entry (findk r m) c ei) in
    (p, l, u)

  let factor_lu x =
    let (p, l, u) = factor_plu x in
    if p = M.id (M.num_rows x) then Some (l, u)
    else None

  let change_basis b c = M.mult (inverse c) b

  let diag mat =
    if not (is_square mat) || is_singular mat then None 
    else
      (* The QR algorithm produces two matrices [d] and [p] such that 
         [a = pdp^-1] and [d] is a diagonal matrix containing the 
         eigenvalues of [mat]. *)
      let rec qr_algo mat q_acc n =
        if n >= 0 then 
          factor_qr mat 
          |> (fun (q, r) -> (M.mult r q, M.mult q q_acc))
          |> fun (m, q_acc) -> (qr_algo m q_acc (n-1))
        else mat, q_acc in
      let d, p = qr_algo mat (M.id (M.num_cols mat)) 25 in
      Some (p, d)

  let eigen mat = 
    match diag mat with
    | None -> None
    | Some (p, d) ->
      begin
        let d' = M.to_column d in
        let p' = M.to_column p in
        let eigen_pair = fun i -> (V.nth (List.nth d' i) i, List.nth p' i) in
        let eigen_list = 
          List.init (M.num_cols mat) (fun i -> i)
          |>List.map eigen_pair in
        Some eigen_list
      end

  (* first n elements in the list or the entire list if n is larger than the
     length of the list *)
  let rec fst_n n base ls = 
    if n <= 0 then List.rev base
    else match ls with
      | [] -> List.rev base
      | hd::tl -> fst_n (n - 1) (hd::base) tl

  let basis k a = 
    let at = M.transpose a in
    let ata = M.mult a at in
    let cols = ata |> M.col_sp |> M.to_column in
    let ls = List.map (fun v -> (V.norm v, v)) cols
             |> List.sort (fun (n1, v1) (n2, v2) -> E.compare n1 n2) in
    fst_n k [] ls |> List.map (fun (_, v) -> v) |> M.concat
end

