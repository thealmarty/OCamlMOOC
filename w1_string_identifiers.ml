let sentence =
  let concat_word = word ^ "," in
  let twice_concat = concat_word ^ concat_word in
  let concat_4 = twice_concat ^ twice_concat in
  concat_4 ^ concat_4 ^ word;;
