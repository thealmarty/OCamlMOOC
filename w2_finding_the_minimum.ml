let rec find_min a i j =
  match a.(i) > a.(j) , (Array.length a -1 < i || Array.length a - 1 < j + 1) with
  |true, true -> a.(j)
  |false, true -> a.(i)
  |true, false -> find_min a j (j + 1)
  |false, false -> find_min a i (j + 1) ;;

let min a =
  find_min a 0 1 ;;

let rec find_min_index a i j =
  match a.(i) > a.(j), (Array.length a - 1 < i || Array.length a - 1 < j + 1) with
  |true, true -> j
  |false, true -> i
  |true, false -> find_min_index a j (j + 1)
  |false, false -> find_min_index a i (j + 1) ;;

let min_index a =
  find_min_index a 0 1 ;;

let it_scales =
  "yes" ;;
