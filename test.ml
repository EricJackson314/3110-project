open OUnit2
open Num
open Vector
open Matrix

module Float = struct
  type t = float
  let add = (+.)
  let add_inv = (~-.)
  let mult = ( *. )
  let mult_inv a = 1. /. a
  let sq_rt = sqrt
  let norm = abs_float
  let one = 1.
  let zero = 0.
  let equals a b = a = b
  let compare = Stdlib.compare
end

(* Helper Functions: *)
(** [almost_equal v1 v2] is true if v1 and v2 are within [1e6] of eachother. *)
let almost_equal v1 v2 = abs_float(v1 -. v2) < 1e-6


(** Creates a list of tests, [tests], for vectors containing floats. *)
module VectorTest(VM : Vector.VectorMaker) = struct
  module V = VM (Float)

  (** [close_vector v1 v2] is true if all the elements of v1 are almost equal to v2. *)
  let close_vector v1 v2 =
    if V.dim v1 <> V.dim v2 then false else
      List.fold_left2 (fun a e1 e2 -> a +. e1 -. e2) 0. (V.to_list v1) (V.to_list v2)
      |> almost_equal 0.

  let zero = Float.zero
  let one = Float.one
  let two = Float.(add one one)
  let three = Float.(add one two)
  let four = Float.(add one three)

  let vec_1 = V.from_list [0.; 1.; 2.]
  let vec_2 = V.from_list [zero; two; four]

  let add_test : test = "add_test" >:: fun _ -> assert_equal (V.add vec_1 vec_1) vec_2

  let gen_l size = List.init size (fun _ -> Random.float 10.)
  let gen_v size = V.from_list (gen_l size)
  let gen_zero size = V.from_list (List.init size (fun _ -> 0.))

  let e1 = gen_v 0
  let e2 = gen_v 0 

  let gen_add_test size (a : unit) : test list = 
    let s = 
      if Random.float 1. > 0.5 then Random.float 10. else Random.float (-10.) in
    let l1 = gen_l size in
    let l2 = gen_l size in
    let sum = List.map2 (fun a b -> a +. b) l1 l2 in
    let sub = List.map2 (fun a b -> a -. b) l1 l2 in
    let v1 = V.from_list l1 in
    let v2 = V.from_list l2 in
    let sl1 = List.map (fun a -> s *. a) l1 in
    let sv1 = V.from_list sl1 in
    let v_add = V.add v1 v2 in
    let v_sub = V.sub v1 v2 in
    let v_sum = V.from_list sum in
    let v_diff = V.from_list sub in
    let dot = List.fold_left2 (fun s a b -> s +. a *. b) 0. l1 l2 in
    let norm = Float.sq_rt (V.dot v1 v1) in
    let norm_v1 = V.normalize v1 in
    let res_v1 = V.scale norm_v1 norm in

    [
      ("gen_sum_test" >:: fun _ -> assert_equal v_add v_sum);
      ("gen_size_test" >:: fun _ -> assert_equal (V.dim v1) size);
      ("gen_sub_test" >:: fun _ -> assert_equal v_diff v_sub);
      ("gen_zero_test" >:: 
       fun _ -> assert_equal (V.scale v1 0.) (gen_zero size));
      ("gen_scale_test" >:: fun _ -> assert_equal (V.scale v1 s) sv1);
      ("gen_dot_test" >:: fun _ -> assert_equal (V.dot v1 v2) dot);
      ("gen_norm_test" >:: fun _ -> assert_equal norm (V.norm v1));
      ("gen_normalize_test" >:: fun _ -> assert_bool "normalize test" (close_vector res_v1 v1));
      ("gen_makers_test" >:: 
       fun _ -> assert_equal v1 (V.make size (fun n -> List.nth l1 n)));
      ("gen_tolist_test" >:: fun _ -> assert_equal l1 (V.to_list v1));
      ("gen_nth_test" >:: fun _ -> 
          assert_equal l1 (List.init size (fun n -> V.nth v1 n)))
    ] 
  let tests =
    add_test::(List.flatten (List.init 5 (fun i -> gen_add_test i ())))
end


(** Creates a list of tests, [tests], for matrices containing floats. *)
module MatrixTest(MM : Matrix.MatrixMaker)= struct
  module M = MM (Float)

  (** [num_rows_tests] tests [Matrix.make] and [Matrix.num_rows]. *)
  let num_rows_tests = List.map 
      (fun i -> "num_rows test" >:: fun _ ->
           assert_equal (M.(make i i (fun i j -> 0.) |> num_rows)) i)
      [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; ]

  (** [num_cols_tests] tests [Matrix.make] and [Matrix.num_cols]. *)
  let num_cols_tests = List.map 
      (fun i -> "num_cols test" >:: fun _ -> 
           assert_equal (M.(make i i (fun i j -> 0.) |> num_cols)) i)
      [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; ]

  (** [from_vector_tests] tests [Matrix.from_vector]. *)
  let from_vector_tests = List.map
      (fun (vec, mat) -> "from_vector test" >:: fun _ ->
           assert_equal (M.from_vector vec) mat)
      [
        (M.V.from_list [1.; 2.; 3.], 
         M.make 3 1 (fun i j -> float_of_int i +.1.));
        (M.V.from_list [2.; 1.; 0.], 
         M.make 3 1 (fun i j -> 2. -. float_of_int i));
        (M.V.from_list [0.; 1.; 4.; 9.; 16.; 25.], 
         M.make 6 1 (fun i j -> (float_of_int i)**2.));
      ]

  (** [entry_tests] tests [Matrix.entry]. *)
  let entry_tests = List.map
      (fun (mat, i, j, v) -> "entry test" >:: fun _ ->
           assert_equal (M.entry i j mat) v)
      [
        (M.make 2 2 (fun i j -> float_of_int (i+j)), 0, 0, 0.);
        (M.make 2 2 (fun i j -> float_of_int (i+j)), 0, 1, 1.);
        (M.make 2 2 (fun i j -> float_of_int (i+j)), 1, 0, 1.);
        (M.make 2 2 (fun i j -> float_of_int (i+j)), 1, 1, 2.);
      ]

  (** [map_tests] tests [Matrix.map]. *)
  let map_tests = List.map
      (fun (mat, f, mat') -> "map test" >:: fun _ ->
           assert_equal (M.map f mat) mat')
      [
        (M.make 2 2 (fun i j -> 1.), 
         (fun x ->x), M.make 2 2 
           (fun i j -> 1.));

        (M.make 2 2 (fun i j -> 1.), 
         (fun x ->2.*.x), 
         M.make 2 2 (fun i j -> 2.));

        (M.make 3 3 (fun i j -> float_of_int i),
         (fun x ->x**2.), 
         M.make 3 3 (fun i j -> (float_of_int i)**2.));
      ]

  (** [id_tests] tests [Matrix.id]. *)
  let id_tests = List.map
      (fun n -> "id test" >:: fun _ ->
           assert_equal (M.id n) (M.make n n (fun i j -> if i=j then 1. else 0.)))
      [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

  (** [to_column_tests] tests [Matrix.to_column]. *)
  let to_column_tests = List.map
      (fun (mat, vecs) -> "to_column test" >:: fun _ ->
           assert_equal (M.to_column mat) vecs)
      [
        (M.make 2 2 (fun i j -> float_of_int i +. float_of_int j), 
         [M.V.from_list [0.; 1.]; M.V.from_list [1.; 2.]]);
        (M.make 2 2 (fun i j -> float_of_int i *. float_of_int j), 
         [M.V.from_list [0.; 0.]; M.V.from_list [0.; 1.]]);
        (M.make 2 2 (fun i j -> (float_of_int (i+1))**2.), 
         [M.V.from_list [1.; 4.]; M.V.from_list [1.; 4.]]);
      ]

  (** [concat_tests] tests [Matrix.concat]. *)
  let concat_tests = List.map
      (fun (vecs, mat) -> "concat_test" >:: fun _ ->
           assert_equal (M.concat vecs) mat)
      [
        ([M.V.from_list [1.; 4.]; M.V.from_list [1.; 4.]], 
         M.make 2 2 (fun i j -> (float_of_int (i+1))**2.));

        ([M.V.from_list [0.; 0.]; M.V.from_list [0.; 1.]],
         M.make 2 2 (fun i j -> float_of_int i *. float_of_int j));

        ([M.V.from_list [0.; 1.]; M.V.from_list [1.; 2.]],
         M.make 2 2 (fun i j -> float_of_int i +. float_of_int j));
      ]

  (** [transpose_tests] tests [Matrix.transpose]. *)
  let transpose_tests = List.map
      (fun (mat, mat') -> "concat test" >:: fun _ ->
           assert_equal (M.transpose mat) mat')
      [
        (M.make 2 2 (fun i j -> float_of_int i),
         M.make 2 2 (fun i j -> float_of_int j));
        (M.make 3 2 (fun i j -> 1.),
         M.make 2 3 (fun i j -> 1.));
      ]

  (** [mult_tests] tests [Matrix.mult]. *)
  let mult_tests = List.map
      (fun (m1, m2, m') -> "mult test" >:: fun _ ->
           assert_equal (M.mult m1 m2) m')
      [
        (M.make 2 2 (fun i j -> 1.),
         M.make 2 2 (fun i j -> 1.),
         M.make 2 2 (fun i j -> 2.));

        (M.make 2 3 (fun i j -> 1.),
         M.make 3 2 (fun i j -> 1.),
         M.make 2 2 (fun i j -> 3.))
      ]

  (** [add_tests] tests [Matrix.add]. *)
  let add_tests = List.map
      (fun (m1, m2, m') -> "add test" >:: fun _ ->
           assert_equal (M.add m1 m2) m')
      [
        (M.make 2 2 (fun i j -> 1.),
         M.make 2 2 (fun i j -> 1.),
         M.make 2 2 (fun i j -> 2.));

        (M.make 2 3 (fun i j -> 2.),
         M.make 2 3 (fun i j -> 3.),
         M.make 2 3 (fun i j -> 5.));
      ]

  (** [nul_sp_tests] tests [Matrix.nul_sp]. *)
  let nul_sp_tests = List.map
      (fun (mat, mat') -> "null_sp test" >:: fun _ ->
           assert_equal (M.nul_sp mat) mat')
      [
        (M.concat 
           [
             M.V.from_list [-3.; 1.; 2.];
             M.V.from_list [6.; -2.; -4.];
             M.V.from_list [-1.; 2.; 5.];
             M.V.from_list [1.; 3.; 8.];
             M.V.from_list [-7.; -1.; -4.];
           ],
         M.concat
           [
             M.V.from_list [2.; 1.; 0.; 0.; 0.];
             M.V.from_list [1.; 0.; -2.; 1.; 0.];
             M.V.from_list [-3.; 0.; 2.; 0.; 1.];
           ])
      ]

  (** [ref_tests] tests [Matrix.ref]. *)
  let ref_tests = List.map
      (fun (mat, mat') -> "ref test" >:: fun _ ->
           assert_equal (M.ref mat) mat')
      [
        (M.make 2 2 (fun i j -> 1.),
         M.make 2 2 (fun i j -> if i = 0 then 1. else 0.));
      ]

  (** [reff_tests] tests [Matrix.ref]. *)
  let rref_tests = List.map
      (fun (mat, mat') -> "rref test" >:: fun _ ->
           assert_equal (M.rref mat) mat')
      [
        (M.id 2, M.id 2);
        (M.id 4, M.id 4);
        (M.id 6, M.id 6);
        (M.concat 
           [
             M.V.from_list [-3.; 1.; 2.];
             M.V.from_list [6.; -2.; -4.];
             M.V.from_list [-1.; 2.; 5.];
             M.V.from_list [1.; 3.; 8.];
             M.V.from_list [-7.; -1.; -4.];
           ], 
         M.concat 
           [
             M.V.from_list [1.; 0.; 0.];
             M.V.from_list [-2.; 0.; 0.];
             M.V.from_list [0.; 1.; 0.];
             M.V.from_list [-1.; 2.; 0.];
             M.V.from_list [3.; -2.; 0.];
           ]);
      ]

  (** [pivot_cols_tests] tests [Matrix.pivot_cols]. *)
  let pivot_cols_tests = List.map
      (fun (mat,  idxs) -> "pivot_cols test" >:: fun _ ->
           assert_equal (M.pivot_cols mat) idxs)
      [
        (M.id 5, [1; 2; 3; 4; 5]);
        (M.concat 
           [
             M.V.from_list [-3.; 1.; 2.];
             M.V.from_list [6.; -2.; -4.];
             M.V.from_list [-1.; 2.; 5.];
             M.V.from_list [1.; 3.; 8.];
             M.V.from_list [-7.; -1.; -4.];
           ],
         [1; 3;]); 
      ]

  (** [col_sp_tests] tests [Matrix.col_sp]. *)
  let col_sp_tests = List.map
      (fun (mat, mat') -> "col_sp test" >:: fun _ ->
           assert_equal (M.col_sp mat) mat')
      [
        (M.id 7, M.id 7);
        (M.id 9, M.id 9);
        (M.concat 
           [
             M.V.from_list [-3.; 1.; 2.];
             M.V.from_list [6.; -2.; -4.];
             M.V.from_list [-1.; 2.; 5.];
             M.V.from_list [1.; 3.; 8.];
             M.V.from_list [-7.; -1.; -4.];
           ], 
         M.concat 
           [
             M.V.from_list [-3.; 1.; 2.];
             M.V.from_list [-1.; 2.; 5.];  
           ]);
      ]

  let tests = List.flatten
      [
        num_rows_tests;
        num_cols_tests;
        from_vector_tests;
        entry_tests;
        map_tests;
        id_tests;
        to_column_tests;
        concat_tests;
        transpose_tests;
        mult_tests;
        add_tests;
        nul_sp_tests;
        ref_tests;
        rref_tests;
        pivot_cols_tests;
        col_sp_tests;
      ]

end

(** Creates a list of tests, [tests], for the MatAlg module. *)
module MatAlgTest(MAM : MatAlg.MatAlgMaker) = struct
  module MA = MAM (Float)

  (** [is_square_test m n b] asserts that the of [is_square] for an [m] by [n] 
      matrix is [b]. *)
  let is_square_test m n b : test =
    "is_square test" >:: fun _ -> 
      assert_equal MA.(is_square (M.make m n (fun i j -> 0.))) b

  (** [is_square_tests] tests [MatAlg.is_square]. *)
  let is_square_tests = List.map (fun (m, n, b) -> is_square_test m n b) 
      [
        (1, 1, true);
        (2, 2, true);
        (1, 2, false);
        (2, 1, false);
        (5, 5, true);
        (10, 9, false);
      ]

  let tests = List.flatten
      [
        is_square_tests;
      ]
end

module V_Test = VectorTest(Vector.Make)
module M_Test = MatrixTest(Matrix.Make)
(* module MatAlg_Test = MatAlgTest(MatAlg.Make) *)

let tests = List.flatten 
    [
      V_Test.tests;
      M_Test.tests;
      (* MatAlg_test.tests; *)
    ]

let suite = "test suite" >::: tests

let _ = run_test_tt_main suite
