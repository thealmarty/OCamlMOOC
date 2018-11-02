(* Let's define some usual arithmetical functions.

    gcd that takes two non-negative integers n and m, and that returns the greatest common divisor of n and m, following Euclid's algorithm.
    multiple_upto : int -> int -> bool that takes two non-negative integers n and r, and that tells whether n admits at least one divisor between 2 and r, inclusive. In other words that there exists a number d >= 2 and <= r, such that the remainder of the division of n by d is zero.
    is_prime a takes a non-negative integer n and checks whether it is a prime number.
*)

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

