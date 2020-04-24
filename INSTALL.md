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

Use the function ```V.normalize u``` to normalize the vector ```u```.  For example,
```ocaml
let u = V.from_list [1.; 4.; 1.; 9.; 1.; ];;
val u : V.t = [1.000000, 4.000000, 1.000000, 9.000000, 1.000000, ]
V.normalize u;;
- : V.t = [0.100000, 0.400000, 0.100000, 0.900000, 0.100000, ]
```

### Matrices

