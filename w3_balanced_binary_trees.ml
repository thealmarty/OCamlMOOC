(*
A binary tree t, of the 'a bt type given in the prelude, is either an empty tree, or the root of a tree with a value and two children subtrees.

1.    Write a function height : 'a bt -> int that computes the height of a tree.
2.    A tree is balanced if, for all internal node n, its two subtrees have the same height. Write a function balanced : 'a bt -> bool that tells if a tree is balanced.

The given prelude
*)
type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;

let rec height t = 
  match t with 
    | Empty -> 0
    | Node (x, y, z) -> 
      max (height x + 1) (height z + 1);;
  
let rec balanced t = 
  match t with
    | Empty -> true
    | Node (x, y, z) -> 
      if height x = height z then balanced x && balanced z                            else false;;
