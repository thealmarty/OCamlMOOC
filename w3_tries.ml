(*
The data structure called trie is very convenient to represent a dictionary whose keys are strings. It is space-efficient way while providing a very fast lookup function.
See the page on WikiPedia.
In this exercise, we will implement such a data structure, assuming that we want to associate integers to the strings of the dictionary.
Let us define a trie using two mutually defined types (given in the prelude):

    trie which represents a trie, that is a tree whose root may contain an integer and whose children are indexed by characters ;
    char_to_children which implements the associative data structure whose keys are characters and whose values are trie (childrens). 

As a trade-off between speed and memory consumption, we choose an associative list to represent the association between characters and children.
The prelude also gives examples of empty trie and of another one that contains the following pairs (key, value):
[("A", 15); ("to", 7); ("tea", 3);("ted", 4); ("ten", 12); ("i", 11); ("in", 5); ("inn", 9)].

1.    Write a function children_from_char : char_to_children -> char -> trie option such that
        children_from_char m c = Some t if (c, t) is the first pair in m with c as a first component ;
        children_from_char m c = None if no such pair exists in m. 
2.    Write a function update_children : char_to_children -> char -> trie -> char_to_children such that
        children_from_char (update_children m c t) c = Some t ;
        children_from_char (update_children m c t) c' = children_from_char m c' for c <> c';
        If children_from_char m c = Some t then List.length (update_children m c t') = List.length m. 
3.    Write a function lookup : trie -> string -> int option such that lookup trie w = Some i if i is the value of the key w in trie and lookup trie w = None if w is not a key of trie.
    To look for a key in a trie, iterate over the characters of the key from left to right. Given the current character c and the current node of the trie n, look for the children n for character c. If such a children exists, continue with that trie and the remainder of the key. If no such children exists, the key is not in the trie. When the characters of the key are entirely consumed, look at the root of the current trie. If there is an integer, this is the value you are looking for. If there is no integer, the key not in the trie.
4.    Write a function insert : trie -> string -> int -> trie such that lookup (insert trie w k) w = Some k and lookup (insert trie w k) w' = lookup trie w' for w <> w'.

The given prelude
*)
type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
	[('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	 ('t',
	  Trie (None,
		[('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
	 ('A', Trie (Some 15, []))])

let rec children_from_char m c =
  match m with
  |(x, t) :: tl -> if x = c then Some t else children_from_char tl c
  |_ -> None;;

let rec update_children m c t = 
  if children_from_char m c = None then (c,t) :: m
  else let rec update_t m c t = 
         match m with
         |(x, old_t) :: tl -> 
             if x = c then (x, t) :: update_t tl c t 
             else (x, old_t) :: update_t tl c t
         |_ -> [] in
    update_t m c t;;

let rec lookup_with_index trie w index =
  match trie with
  | Trie (key, ctc) -> 
      if (index + 1 <= String.length w) then 
        match (children_from_char ctc w.[index]) with
        | Some trie -> lookup_with_index trie w (index + 1)
        | None -> None
      else key;;

let rec lookup trie w =
  lookup_with_index trie w 0;;

let rec insert_with_index trie w v index = 
  match trie with
  | Trie (key, ctc) -> 
      if (index + 1 <= String.length w) then
        match (update_children ctc w.[index]) with
        | 
          | None -> 
      else Trie (Some v, ctc)
