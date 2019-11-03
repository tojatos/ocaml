let print_list_string xs = print_endline (String.concat " " xs);;
let print_list_char xs = print_list_string (List.map (String.make 1) xs);;
let print_list_int xs = print_list_string (List.map string_of_int xs);;
let print_int_endline x = print_int x; print_endline "";;
let print_bool_endline x = print_endline (string_of_bool x);;
let print_float_endline x = print_endline (string_of_float x);;
let print_tuple_endline = function (a, b) ->
  print_int a; print_string " "; print_int b; print_endline "";;

print_endline "## Zad 1";;
let moduloList (xs, x) = List.filter (fun s -> s mod x = 0) xs;;
print_list_int (moduloList ([1;10;7;8;15], 5));;
print_endline "## Zad 4";;
let rec (%%) a b =
  if b = 0 then a else b %% (a mod b);;
print_int_endline (15 %% 60);;
print_int_endline (30 %% 12);;
