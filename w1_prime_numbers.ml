let rec gcd n m =
  match n mod m with
  |0 -> m
  |_ -> gcd m (n mod m) ;;

let rec multiple_upto n r =
  if r >= 2 then
    (match n mod r with
     |0 -> true
     |_ -> multiple_upto n (r-1))
  else
    false ;;

let rec is_prime n =
  match n with
  |1 -> false
  |_ -> not (multiple_upto n (n-1)) ;;

