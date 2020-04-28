# Charter 

Ralph Wang ```rw474```

Eric Jackson ``` ebj29```


## Status Meetings

We will meet via Zoom on Tuesdays, and Thursdays at 11:00AM EST and Saturdays at 12:30 EST.

## Proposal

### Summary
We will design a Linear Algebra library, which we will use to implement image compression and the Eigenfaces algorithm.

### Key Features
1. Linear Algebra Library
2. ```.img``` Parser 
3. Image Compression
4. Eigenfaces

### Description
Our goal is to design a robust and efficient linear algebra library, including methods for matrix multiplication, diagonalization, computing eigenvalues and eigenvectors, computing null-space and column-space, row reduction, and so on. We will use this library to perform image compression and eigenface analysis. Performance on these tasks will measure the efficiency of our implementation. We plan to use pictures of Cornell CS Faculty for both the image compression, as well as the Eigenface algorithms.
## Road Map

### MS1 (Beta)

Satisfactory | Good | Excellent
--------------- | ------- | --------------
implementation of matrix multiplication, null space, column space, row reduction, and other basic matrix operations<br> implementation of eigenvalues, eigenvectors, diagonalization, and the other matrix operations<br> |  write an image file parser to extract and store image data and an image file generator to generate an image from given data <br> optimize and performance test the matrix operations <br> |  write the image compression algorithm, leaving compression parameters and basis choice flexible<br> optimize compression parameters and basis choice<br>

### MS2 (Release)
Satisfactory | Good | Excellent
--------------- | ------- | --------------
 finish excellent scope from MS1<br> implement eigenfaces algorithm <br> | <br> <br> |  <br> <br>

## Design Sketch

### Modules

* Vector
* Matrix
* Image
* EigenFaces

### Data Structures
Tentatively, we are planning to use arrays to implement vectors and matrices. We agree to not mutate them.

### Third-Party Libraries
We are not planning to use any third-party libraries. If we decide to implement a GUI, this may change.

### Testing
We will need to write thorough tests for the Vector and Matrix modules. We plan to test against a pre-existing library, or to create tests using Python's Numpy library.
Testing of Image Compression and EigenFace performance will be done by human inspection.

## Team Expectations

* We both show equal commitment to our objective.
* We both take part in deciding how work should be allocated.
  - Volunteer system
* We are both committed to helping each other learn.
* We acknowledge good contributions from team members.
* We handle disagreements and conflicts constructively within the team.
* We are able to give constructive criticism to one another and to accept it ourselves.
* We all turn up to meetings and stay to the end.
* We are good at making sure that we both know what’s going on.
	- We will use a review branch to ensure that we are both on the same page with what is being done.
* When one of us is under pressure, others offer to help them.
* We trust each other.
* We remain united even when we disagree.
* We support each other to outsiders.
* We feel comfortable and relaxed with one another.

signed, Eric Jackson. 7 April 2020.
signed, Ralph Wang. 7 April 2020.