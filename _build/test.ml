open OUnit2
open Num
open Vector
open Matrix

module Float : Num = struct
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

(** Creates a list of test, [test], for vectors containing floats. *)
module VectorTest(VM : Vector.VectorMaker) = struct
  module V = VM (Float)

  let zero = Float.zero
  let one = Float.one
  let two = Float.(add one one)
  let three = Float.(add one two)
  let four = Float.(add one three)

  let vec_1 = V.from_list [zero; one; two]
  let vec_2 = V.from_list [zero; two; four]

  let add_test : test = "add_test" >:: fun _ -> assert_equal (V.add vec_1 vec_1) vec_2


  let tests =
    [
      add_test
    ]
end

module VTest = VectorTest(Vector.Make)

let tests = List.flatten 
    [
      VTest.tests;
    ]

let suite = "test suite" >::: tests

let _ = run_test_tt_main suite