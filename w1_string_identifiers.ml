(*
Suppose that a variable word exists and is a string.
Define a variable sentence that uses 5 string concatenations to create a string containing 9 times word, separated by commas (',').
This time, experiment with defining local let ... ins to store the partial results.
*)

let sentence =
  let concat_word = word ^ "," in
  let twice_concat = concat_word ^ concat_word in
  let concat_4 = twice_concat ^ twice_concat in
  concat_4 ^ concat_4 ^ word;;
