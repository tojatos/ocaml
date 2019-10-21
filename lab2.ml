let print_list_string xs = print_endline (String.concat " " xs);;
let print_list_char xs = print_list_string (List.map (String.make 1) xs);;
let print_list_int xs = print_list_string (List.map string_of_int xs);;
let print_int_endline x = print_int x; print_endline "";;
let print_bool_endline x = print_endline (string_of_bool x);;
let print_float_endline x = print_endline (string_of_float x);;

print_endline "## Zad 1";;
let rec polaczListy (xs, ys) =
  match (xs, ys) with
    | ([], _) -> ys
    | (_, []) -> xs
    | (h::t, ys) -> h :: polaczListy(ys, t)
;;

print_list_int (polaczListy ([],[]));;
print_list_int (polaczListy ([1],[]));;
print_list_int (polaczListy ([],[2]));;
print_list_int (polaczListy ([1;2;3;4;5],[6;7;8;9;10]));;
print_list_int (polaczListy ([1;2;3;4;5],[6;7]));;
print_list_int (polaczListy ([3;4;5],[6;7;8;9;10]));;
print_list_int (polaczListy ([1;8;7],[9;5;12;2;1]));;

print_endline "## Zad 2";;
let listForN (n, m) =
  let rec listIter (xs, last, n, m) =
    if m <= 0 then xs
    else last :: listIter (xs, last + n, n, m - 1)
  in listIter ([], 0, n, m)
;;

print_list_int (listForN(3, 4));;
print_list_int (listForN(6, 2));;
