open Csv

(** [get_csv file] is the array representing the contents of [file]. *)
let get_csv (file : string) =
  Csv.load(file)
  |> Csv.to_array
  |> Array.map (fun i -> (Array.map (fun j -> float_of_string j) i))
