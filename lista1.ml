let print_list_string xs = print_endline (String.concat " " xs);;
let print_list_char xs = print_list_string (List.map (String.make 1) xs);;
let print_list_int xs = print_list_string (List.map string_of_int xs);;
let print_int_endline x = print_int x; print_endline "";;
let print_bool_endline x = print_endline (string_of_bool x);;

print_endline "## Zad 1";;
let list1 = [[5;6];[1;2;3]];;
let list2 = [['f';'f'];['c';'d';'v']];;
let rec flatten xss =
  if xss = [] then []
  else List.hd xss @ flatten (List.tl xss);;
print_list_int (flatten list1);;
print_list_char (flatten list2);;

print_endline "## Zad 2";;
let rec count (x, xs) =
  if xs = [] then 0
  else (
   ( if List.hd xs = x then 1 else 0 ) + count (x, List.tl xs)
  );;
print_int_endline (count (1, [3;4;1]));;
print_int_endline (count (1, [3;4;2]));;
print_int_endline (count ('a', ['a';'l';'a']));;

print_endline "## Zad 3";;
let rec replicate (x, n) =
  if n <= 0 then []
  else x :: replicate(x, n-1);;
print_list_string (replicate ("la", 3));;
print_list_int (replicate (6, 8));;

print_endline "## Zad 4";;
let rec sqrList xs =
  if xs = [] then []
  else List.hd xs * List.hd xs :: sqrList (List.tl xs);;
print_list_int (sqrList [2;3;5;-4;0]);;

print_endline "## Zad 5";;
let palindrome xs = xs = List.rev xs;;
print_bool_endline (palindrome []);;
print_bool_endline (palindrome [2;3;5;-4;0]);;
print_bool_endline (palindrome ['a';'l';'a']);;

print_endline "## Zad 6";;
let rec listLength xs =
  if xs = [] then 0
  else 1 + listLength (List.tl xs);;
print_int_endline (listLength []);;
print_int_endline (listLength [2;3;5;-4;0]);;
print_int_endline (listLength ['a';'l';'a']);;
