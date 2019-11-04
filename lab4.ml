open Printlib;;

print_endline "## Zad 1";;

let niemalejaca xs =
  if xs = [] then true
  else snd (List.fold_left (fun (last, pred) x -> (x, if pred then x >= last else false)) (List.hd xs, true) xs)
;;
let tylkoNiemalejace xss = List.filter (fun xs -> niemalejaca xs) xss;;

print_list_list_int (tylkoNiemalejace [[1;1;3;1;2]; [2]; [3;4;1]; []]);;

print_endline "## Zad 2";;

let foo (acc, ys) x =
    if ys = [] then (acc, [])
    else ((x, List.hd ys)::acc, List.tl ys)
;;
let zip (xs, ys) =
  List.rev (fst (List.fold_left foo ([], ys) xs));;

zip ([1;2;3], ['a';'b';'c']);;
zip ([1;2;3], ['a';'b']);;
zip ([1;2], ['a';'b';'c']);;


