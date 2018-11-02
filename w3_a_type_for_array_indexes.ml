(*
The previous week, we asked you the following question: Consider a non empty array of integers a, write a function min_index : int array -> int that returns the index of the minimal element of a.
As the arrays contain integers and the indices of arrays are also represented by integers, you might have confused an index and the content of a cell. To avoid such a confusion, let us define a type for index (given in the prelude below).
This type has a single constructor waiting for one integer.
For instance, if you want to represent the index 0, use the value Index 0.
Defining such a type is interesting because it allows the type-checker to check that an integer is not used where an index is expected (or the converse).

1.    Write a function read : int array -> index -> int such that read a (Index k) returns the k-th element of a.
2.    Write a function inside : int array -> index -> bool such that inside a idx is true if and only if idx is a valid index for the array a.
3.    Write a function next : index -> index such that next (Index k) is equal to Index (k + 1).
4.    Consider a non empty array of integers a, write a function min_index : int array -> index that returns the index of the minimal element of a.

The given prelude
*)
type index = Index of int

let read (a: int array) (index: index) =
  match index with Index k -> a.(k);;

let inside (a : int array) (index: index) =
  index >= Index 0 && index < Index (Array.length a);;

let next (index: index) = match index with Index k -> Index (k + 1);;

let rec find_min_index a n =
  if a.(n) = (Array.fold_left min max_int a) then Index n
  else find_min_index a (n+1);;

let min_index a =
  find_min_index a 0;;
