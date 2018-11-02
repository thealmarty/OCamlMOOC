(*
A queue is a standard FIFO data structure. See wikipedia

In this exercise, we implement a queue with a pair of two lists (front, back) such that front @ List.rev back represents the sequence of elements in the queue.

1.    Write a function is_empty : queue -> bool such that is_empty q is true if and only if q has no element.
2.    Write a function enqueue : int -> queue -> queue such that enqueue x q is the queue as q except that x is at the end of the queue.
3.    Write a function split : int list -> int list * int list such that split l = (front, back) where l = back @ List.rev front and the length of back and front is List.length l / 2 or List.length l / 2 + 1
4.    Write a function dequeue : queue -> int * queue such that dequeue q = (x, q') where x is the front element of the queue q and q' corresponds to remaining elements. This function assumes that q is non empty.

The given prelude
*)
type queue = int list * int list

let is_empty (front, back) =
  (front, back)= ([],[]);;

let enqueue (x: int) (front, back) =
  (front, x :: back);;

let enqueue_front x (front, back) =
  (front @ [x], back);;

let rec split_list l (front, back) count half_length =
  if count <= (half_length - 1) then
    match l with
    |hd :: tl -> enqueue hd (split_list tl (front, back) (count + 1) half_length)
    |_ -> ([],[])
  else 
    match l with
    |hd :: tl -> enqueue_front hd (split_list tl (front, back) count half_length)
    |_ -> ([],[]);;

let split l =
  split_list l ([],[]) 0 (List.length l / 2);;

let dequeue ((front, back): int list*int list) =
  match (front, back) with
  |(hd :: tl, back) -> (hd, (tl, back))
  |([], back) -> ((List.hd (List.rev back)), ([], (List.rev (List.tl (List.rev back)))));;

