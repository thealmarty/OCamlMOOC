(* Suppose that a variable x exists and is an integer.
Define a variable x_power_8 that uses three multiplications to calculate x to the power of 8. The only function you are allowed to call is the * operator.
The given prelude: *)

let x = Random.int 9 + 1 (* not 0 *)

let x_square = x * x ;;
let x_power_4 = x_square * x_square ;;
let x_power_8 = x_power_4 * x_power_4 ;;
