open Batteries.LazyList

let rec ilazy my_map f =
  function
  | Nil -> Nil
  | Cons (x, xs) -> Cons ((f x), my_map f xs)


(* let ilazy l = Cons (2, Nil) *)

(* let l = Cons (2, Nil) *)


let rec string_of_stream pr = function
  | Nil -> ""
  | Cons (x, lazy xs) -> (pr x) ^ (string_of_stream pr xs)

let sqr x = x * x

let _ =
  print_endline @@ string_of_stream string_of_int l;
  print_endline @@ string_of_stream string_of_int (my_map sqr l)
