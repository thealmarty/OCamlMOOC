let exchange k =
  int_of_string (Char.escaped (String.get (string_of_int k) 1) ^ Char.escaped (String.get (string_of_int k) 0)) ;; 

let is_valid_answer (grand_father_age, grand_son_age) =
  (grand_father_age = 4 * grand_son_age) && ((exchange grand_father_age) * 3 = (exchange grand_son_age)) ;; 

let rec find (max_grand_father_age,min_grand_son_age) = 
  if max_grand_father_age / 4 >= min_grand_son_age then
    (if is_valid_answer (max_grand_father_age, max_grand_father_age / 4) then 
       (max_grand_father_age, max_grand_father_age / 4) else find (max_grand_father_age - 1, min_grand_son_age))
  else (-1,-1) ;;

