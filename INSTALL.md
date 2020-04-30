# Instructions for Install and Build

Run ```make demo``` to open utop with demo code preloaded.


## Demo

The demo comes with vectors and matrices containing float elements.


### Vectors

Use the function ```V.from_list lst``` a new vector and the function ```V.dot u v``` to compute the dot product. For example,
```ocaml
let u = V.from_list [50.; 40.; 10.; 11.; ]
and v = V.from_list [20.; 25.; 100.; 10.; ];;
val u : V.t = [50.000000, 40.000000, 10.000000, 11.000000, ]
val v : V.t = [20.000000, 25.000000, 100.000000, 10.000000, ]
V.dot u v;;
- : float = 3110.
```

Use the function ```V.normalize u``` to normalize the vector ```w```.  For example,
```ocaml
let w = V.from_list [1.; 4.; 1.; 9.; 1.; ];;
val w : V.t = [1.000000, 4.000000, 1.000000, 9.000000, 1.000000, ]
V.normalize w;;
- : V.t = [0.100000, 0.400000, 0.100000, 0.900000, 0.100000, ]
```

### Matrices

The Matrix module contains functions for simple matrix computations. For example,

```ocaml
let a = M.make 3 3 (fun i j -> float_of_int (i + 1 + j * 3))
and b = M.make 3 3 (fun i j -> float_of_int (3 * i + 2 + 4*j));;
val a : M.t = ...
val b : M.t = ...
M.mult a b;;
- : M.t = ...
```



### MatAlg

The MatAlg module contains more complex functions. Use ```MA.ortho_normal a``` to compute the matrix ```a'```, where the columns of ```a'``` form an ortho-normal basis for the column space of ```a```. For example,

```ocaml
let a = M.make 4 4 (fun i j -> float_of_int (i*i - j + 1));;
val a : M.t = ...
MA.ortho_normal a;;
- : M.t = ...
```

Use the function ```MA.row_sp b``` to compute the matrix ```b'```, where the columns of ```b'``` span the row space of ```b```. For example,

```ocaml
let b = M.make 5 5 (fun i j -> float_of_int (i + j));;
val b : M.t = ...
MA.row_sp b;;
- : M.t = ...
```

### Additional Functions

There are a number of other functions not listed in this document. Run ```make docs-public``` to generate documentation and read about additional functionality.