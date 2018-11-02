(*
In this exercise, we implement the classic functions over lists.

1.    Write a function mem : int -> int list -> bool such that mem x l is true if and only if x occurs in l.
2.    Write a function append : int list -> int list -> int list such that append l1 l2 is the concatenation of l1 and l2.
3.    Write a function combine : int list -> int list -> (int * int) list such that combine l1 l2 is the list of pairs obtained by joining the elements of l1 and l2. This function assumes that l1 and l2 have the same length. For instance, combine [1;2] [3;4] = [(1, 3); (2, 4)].
4.    Write a function assoc : (string * int) list -> string -> int option such that assoc l k = Some x if (k, x) is the first pair of l whose first component is k. If no such pair exists, assoc l k = None.
*)

let rec mem (x:int) l =
  match l with
  |hd :: tl -> if x = hd then true else mem x tl
  |[] -> false;;

let rec append l1 l2 =
  l1 @ l2;;

let rec combine (l1: int list) (l2: int list) =
  match l1,l2 with
  |hd1 :: hd12 :: tl1, hd2 :: hd22 :: tl2 -> (hd1,hd2) :: combine (hd12 :: tl1) (hd22 :: tl2)
  |hd1 :: [], hd2 :: [] -> [(hd1,hd2)]
  |[],[] -> [];; 

let rec assoc (l: (string * int) list) (k: string) =
  match l with
  |(s, x) :: tl -> if k = s  then Some x else assoc tl k
  |_ -> None;;


