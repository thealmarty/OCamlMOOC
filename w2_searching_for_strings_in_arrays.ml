(*    1. Write a function is_sorted : string array -> bool which checks if the values of the input array are sorted in strictly increasing order, implying that its elements are unique (use String.compare).
      2. Using the binary search algorithm, an element can be found very quickly in a sorted array.
    Write a function find : string array -> string -> int such that find arr word is the index of the word in the sorted array arr if it occurs in arr or -1 if word does not occur in arr.
    The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity. Beware that you really perform the minimal number of accesses. For instance, if your function has to test the contents of a cell twice, be sure to put the result of the access in a variable, and then perform the tests on that variable.
*)

let rec find_inner dict word i_t i_t1 =
  let larger_t2 = i_t1 + (if abs (i_t - i_t1) / 2 = 0 then 1 else abs (i_t - i_t1) / 2) and
      smaller_t2 = i_t1 - (if abs (i_t - i_t1) / 2 = 0 then 1 else abs (i_t - i_t1) / 2)
  in
    match dict.(i_t1) <= word, (larger_t2 <= Array.length dict - 1 && smaller_t2 >= 0) with
      |true, true -> if dict.(i_t1) = word then i_t1 else find_inner dict word i_t1 larger_t2
      |false, true -> find_inner dict word i_t1 smaller_t2
      |true, false -> if dict.(i_t1) = word then i_t1 else -1
      |false, false -> -1
;;

let find dict word =
  match dict with
    |[||] -> -1
    |[|x|] -> if x = word then 0 else -1
    |_ -> 
      if dict.((Array.length dict - 1) / 2) = word then ((Array.length dict - 1) / 2) 
      else if dict.((Array.length dict - 1) / 2) > word then
        find_inner dict word ((Array.length dict - 1) / 2) ((Array.length dict - 1) / 4)
      else
        find_inner dict word ((Array.length dict - 1) / 2) ((Array.length dict - 1) / 2 + ((Array.length dict - 1) / 4))
;;
