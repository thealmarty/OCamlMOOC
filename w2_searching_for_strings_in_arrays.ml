(*    1. Write a function is_sorted : string array -> bool which checks if the values of the input array are sorted in strictly increasing order, implying that its elements are unique (use String.compare).
      2. Using the binary search algorithm, an element can be found very quickly in a sorted array.
    Write a function find : string array -> string -> int such that find arr word is the index of the word in the sorted array arr if it occurs in arr or -1 if word does not occur in arr.
    The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity. Beware that you really perform the minimal number of accesses. For instance, if your function has to test the contents of a cell twice, be sure to put the result of the access in a variable, and then perform the tests on that variable.
*)

let rec is_bigger a i = 
  if a.(i + 1) > a.(i) && i < Array.length a - 2 then is_bigger a (i + 1) 
  else i = Array.length a - 2 && a.(i + 1) > a.(i) ;;

let is_sorted a =
  match Array.length a with
  |0 -> true
  |1 -> true
  |_ -> is_bigger a 0 ;; 

(*l is the left-most index find_in looks for word in, r-1 is the right-most index find_in looks for word in. *)
let rec find_in arr l r word =
  if l < r then
    (let mid = (l + r)/2 and mid_ele = arr.((l+r)/2) in
      if mid_ele < word then find_in arr (mid + 1) r word
      else if mid_ele > word then find_in arr l mid word
      else mid)
  else -1 ;;

let find arr word =
  find_in arr 0 (Array.length arr) word ;;

