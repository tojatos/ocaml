let print_list_string xs = print_endline (String.concat " " xs);;
let print_list_char xs = print_list_string (List.map (String.make 1) xs);;
let print_list_int xs = print_list_string (List.map string_of_int xs);;
let print_int_endline x = print_int x; print_endline "";;
let print_bool_endline x = print_endline (string_of_bool x);;
let print_float_endline x = print_endline (string_of_float x);;
let print_tuple = function (a, b) ->
  print_string "("; print_int a; print_string ","; print_int b; print_string ") ";;
let print_tuple_endline = function (a, b) ->
  print_tuple (a, b); print_endline "";;
let rec print_list_tuple_endline xs = match xs with
  | [] -> print_endline ""
  | h::t -> print_tuple h; print_list_tuple_endline t;;
let rec print_list_list_int xss = match xss with
  | [] -> print_endline ""
  | h::t -> print_list_int h; print_list_list_int t;;
