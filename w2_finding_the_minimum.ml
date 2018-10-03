let min =
  Array.fold_left (fun x y -> if x < y then x else y) max_int ;;

let rec find_min_index a i = 
  if a.(i) = min a then i else find_min_index a (i + 1) ;;
 
let min_index a =
  find_min_index a 0 ;;

let it_scales =
  "no" ;;

