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
  val row_sp : matrix -> matrix
  val perp : matrix -> matrix
  (* old_basis -> new_basis -> conversion_matrix *)
  val change_basis : matrix -> matrix -> matrix
  val det : matrix -> elem
  val is_singular : matrix -> bool
  val inverse : matrix -> matrix
  val factor_lu : matrix -> (matrix * matrix) option
  val factor_plu : matrix -> matrix * matrix * matrix
  val eigen : matrix -> (elem * (vector list)) list
  val diag : matrix -> (matrix * matrix) option
  val svd : matrix -> matrix * matrix * matrix
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

  let is_square mat = M.num_rows mat = M.num_cols mat

  (** [gram_schmidt v x] is the vector [x'] which is the component of [x]
      orthogonal to the vectors in [v]. *)
  let gram_schmidt v x = 
    if v = [] then x else
      let sclr x' v' = 
        M.V.(scale v' (E.mult (dot x v') (E.mult_inv (dot v' v')))) in 
      List.fold_left (fun x' v' -> M.V.sub x' (sclr x' v') ) x v

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

  let eigen = (fun _ -> failwith "Unimplemented")

  let diag = (fun _ -> failwith "Unimplemented")

  let svd = (fun _ -> failwith "Unimplemented")
end

