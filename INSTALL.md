# Instructions for Install and Build

Hi! To get started, first run ```make```. This will install an ocaml package required by our project. Then, run ```make demo``` to open utop with our demo code preloaded.


## Demo
The demo comes with vectors and matrices containing real and complex elements.

### Real Vectors
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

### Real Matrices
The Matrix module contains functions for simple matrix computations. For example,

```ocaml
let mat_1 = M.make 3 3 (fun i j -> float_of_int (i + 1 + j * 3))
and mat_2 = M.make 3 3 (fun i j -> float_of_int (3 * i + 2 + 4*j));;
val mat_1 : M.t = ...
val mat_2 : M.t = ...
M.mult mat_1 mat_2;;
- : M.t = ...
```

### MatAlg
The MatAlg module contains more complex functions. Use ```MA.ortho_normal a``` to compute the matrix ```a'```, where the columns of ```a'``` form an ortho-normal basis for the column space of ```a```. For example,

```ocaml
let mat_3 = M.make 4 4 (fun i j -> float_of_int (i*i - j + 1));;
val mat_3 : M.t = ...
MA.ortho_normal mat_3;;
- : M.t = ...
```

Use the function ```MA.row_sp b``` to compute the matrix ```b'```, where the columns of ```b'``` span the row space of ```b```. For example,

```ocaml
let mat_4 = M.make 5 5 (fun i j -> float_of_int (i + j));;
val mat_4 : M.t = ...
MA.row_sp mat_4;;
- : M.t = ...
```

### Complex Vectors and Matrice
The complex-valued vectors and matrices behave similarly to the real-valued vectors and matrices, except you must use the modules ```V_complex```, ```M_Complex```, and ```MA_Complex```. For example, use the function ```V_complex.dot``` to compute the dot product of two complex vectors.

```ocaml
V_complex.dot a b;;
```
Use the method ```MA_complex.inverse``` to compute the inverse of a complex matrix.
```ocaml
MA_complex.inverse matrix_6;;
```

### Additional Functions
There are a number of other functions not listed in this document. Run ```make docs-public``` to generate documentation and read about additional functionality.

### EigenFaces
After constructing a linear algebra library, we put it to good work. Run ```make faces``` to get our facial recognition software started and then come back here to read about it while it works.

The program works by creating a "face"-space from a set of training photos. Then, it takes new photos and projects them onto this space. Using this projection, it calculates a number for each image and compares it to the numbers assigned to the training faces. The program then guesses that the new image is a picture of the person in the training image with the nearest assigned number.

While our program is certainly not powerful enough to play big brother, it does demonstrate an interesting application of linear algebra to a fun problem while testing the computational efficiency of our project.
