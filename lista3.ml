open Printlib;;

(*
## Zad 1
val f1 : (int -> int -> 'a) -> 'a = <fun>
val f2 : (string -> 'a) -> string -> string -> 'a = <fun>
*)

print_endline "## Zad 2";;
let curry3 f x y z = f (x, y, z);;
let uncurry3 f (x, y, z) = f x y z;;

let add x y z = x + y + z;;
let mul (x, y, z) = x * y * z;;
let add_uncurry (x, y, z) = uncurry3 add (x, y, z);;
let mul_curry x y z = curry3 mul x y z;;
print_int_endline (add 3 4 5);;
print_int_endline (add_uncurry (3, 4, 5));;
print_int_endline (mul (3, 4, 5));;
print_int_endline (mul_curry 3 4 5);;

print_endline "## Zad 3";;
let rec sumProd xs =
  match xs with
    | h::t -> let (s,p) = sumProd t
      in (h+s, h*p)
    | [] -> (0,1);;
let mySumProd xs = List.fold_left (fun (a, b) x -> (a+x, b*x)) (0,1) xs;;

print_tuple_endline (sumProd [1;3;2;-2;3]);;
print_tuple_endline (mySumProd [1;3;2;-2;3]);;

(*
## Zad 4
a) infinite loop in `quicksort large`
b) duplicate elements may be not present in sorted list
*)

print_endline "## Zad 5";;
let insertSort f xs =
  let rec insert elem = function
    | [] -> [elem]
    | h::t as ys -> if f h elem then elem::ys
        else h::(insert elem t)
  in List.fold_left (fun acc elem -> insert elem acc) [] xs;;

let rec merge f = function
  | ([], ys) -> ys
  | (ys, []) -> ys
  | ((h1::t1 as xs), (h2::t2 as ys)) ->
      if f h1 h2 then h2 :: merge f (xs, t2)
      else h1 :: merge f (t1, ys)
;;

let split xs =
  let rec helper (n, zs, ys) = 
    if n = 0 then (List.rev zs, ys)
    else helper (n-1, List.hd ys :: zs, List.tl ys)
  in helper (List.length xs / 2, [], xs)
;;

let rec mergeSort f = function
    | [] -> []
    | [x] -> [x]
    | xs -> match split xs with
        (left, right) ->  merge f (mergeSort f left, mergeSort f right)
;;
let desc a b = fst a < fst b;;
let asc a b = fst a > fst b;;
let list = [(5,0);(-8,1);(3,2);(74,3);(3,4);(54,5)];;
print_list_tuple_endline (insertSort desc list);;
print_list_tuple_endline (insertSort asc list);;
(* print_list_int (merge (fun a b -> a > b) ([1;3;6;7;10], [0;2;6;6;8;21]));; *)
(*
print_list_int (fst (split [1;3;6;7;10]));;
print_list_int (snd (split [1;3;6;7;10]));;
print_list_int (fst (split [3;6;7;10]));;
print_list_int (snd (split [3;6;7;10]));;
print_list_int (fst (split []));;
print_list_int (snd (split []));;
print_list_int (fst (split [1]));;
print_list_int (snd (split [1]));;
*)
print_list_tuple_endline (mergeSort desc list);;
print_list_tuple_endline (mergeSort asc list);;
