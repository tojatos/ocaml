let print_list_string xs = print_endline (String.concat " " xs);;
let print_list_char xs = print_list_string (List.map (String.make 1) xs);;
let print_list_int xs = print_list_string (List.map string_of_int xs);;
let print_int_endline x = print_int x; print_endline "";;
let print_bool_endline x = print_endline (string_of_bool x);;
let print_float_endline x = print_endline (string_of_float x);;

print_endline "## Zad 1";;
let funcA (x, y) =
  if x then 5
  else if y then 2
  else 0
;;

let funcB x = (x +. 1.2, x /. 5.4);;
let funcC (x, y) =
  if List.hd x = 5 then [y]
  else [y]
;;


print_int_endline (funcA (true, true));;
print_int_endline (funcA (false, true));;
print_int_endline (funcA (true, true));;
print_int_endline (funcA (false, false));;

print_endline "## Zad 2";;
let rec getN (xs, n) =
  if xs = [] then raise (Failure "Poza zakresem")
  else if n = 0 then List.hd xs
  else getN (List.tl xs, n-1)
;;
print_int_endline (getN ([1;3;5;7], 2));;
(* getN([], 2);; *)
(* getN([1], 1);; *)
print_int_endline (getN([1], -1));;

print_endline "## Zad 3";;
let rec pairChange xs =
  if xs = [] then []
  else if List.tl xs = [] then [List.hd xs]
  else List.hd (List.tl xs) :: List.hd xs :: pairChange (List.tl (List.tl xs));;

print_list_int (pairChange [1;3;2;5;4]);;
