(* Let's define two functions working with strings:*)

(* 1.last_character that returns the last character of a string, assuming that the string argument is not empty. *)
let last_character str =
  String.get str (String.length str - 1) ;;

(* 2.string_of_bool that converts a boolean value to its string representation. *)
let string_of_bool truth =
  if truth then "true" else "false" ;;
