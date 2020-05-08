#directory "_build";;
#require "camlimages";;
#require "camlimages.all_formats";;
#load_rec "MatAlg.cmo";;
#load_rec "Reader.cmo";;
#load_rec "Writer.cmo";;
#load_rec "Img.cmo";;

open Num;;
open MatAlg;;
open Printf;;

module MA = Img.MA;;
module V = MA.V;;
module M = MA.M;;
(* #install_printer MA.V.format;;
   #install_printer MA.M.format;; *)

(** If [mat] is an n-by-n matrix, then [mat_to_vec mat] is an n^2-dimensional
    vector resulting from catenating the columns of [mat]. *)
let mat_to_vec mat =
  let dim = (M.num_rows mat) in
  V.make (dim*dim) (fun  i -> M.entry (i mod dim) (i / dim) mat)

(** [average vecs] is the vector where the ith entry is the average of the 
    ith entries of the vectors in vecs.
    Requires: [vecs] is a non-empty list of vectors of the same size. *)
let average vecs =
  let n = List.length vecs in
  let sum = match vecs with
    | h::t -> List.fold_left (V.add) h t
    | [] -> failwith "empty list!" in
  V.make (V.dim sum) (fun i -> (V.nth sum i) /. float_of_int n)

(** If [mats] contains m n-by-n matrices, then [construct_C mats] is a 
    m-by-n^2 matrix where the ith column represents the ith matrix in [mats]
    minus the average of all the matrices in [mats].
    Requires: [mats] is a non-empty list of matrices of the same size. *)
let construct_C mats =
  let vecs = List.map mat_to_vec mats in
  let avrg = average vecs in
  let ajst = List.map (fun v -> V.sub v avrg) vecs in
  M.concat ajst, avrg

(** *)
let eigen_faces () =
  let get_img name = Img.load ("images/" ^ name ^ ".bmp") |> Img.as_matrix in
  let mats = List.map get_img ["foster-3"; "gries-3"; "nye"; "pollack-2"] in
  let c, avrg = construct_C mats in
  let m = M.mult (M.transpose c) c in
  let eigens =
    M.make 
      (M.num_rows m) 
      (M.num_cols m) 
      (fun i j -> M.entry i j m +. (if i = j then 1. else 0.))
    |> MA.eigen
    |> (fun i -> match i with None -> failwith "boring" | Some lst -> lst)
    |> List.map (fun i -> match i with (e, v) -> 
        (e, List.hd (M.to_column (M.scale e (M.mult c (M.from_vector v))))))
  in eigens, avrg

(**  *)
let proj v eigs = List.fold_left 
    (fun acc (eig_val, eig_vec) -> 
       acc +. (( eig_val *. (V.dot v eig_vec)))) 0. eigs


let main () =
  let get_img name = 
    Img.load ("images/" ^ name ^ ".bmp") 
    |> Img.as_matrix
    |> mat_to_vec in
  let vecs = List.map get_img ["foster-3"; "gries-3"; "nye"; "pollack-2"] in
  let eigs, avrg = eigen_faces () in
  let foster2 = ((fun v -> proj(V.sub v avrg) eigs) (get_img "foster-4")) in
  (List.map (fun v -> proj (V.sub v avrg) eigs) vecs, foster2)