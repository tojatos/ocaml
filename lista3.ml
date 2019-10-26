let print_list_string xs = print_endline (String.concat " " xs);;
let print_list_char xs = print_list_string (List.map (String.make 1) xs);;
let print_list_int xs = print_list_string (List.map string_of_int xs);;
let print_int_endline x = print_int x; print_endline "";;
let print_bool_endline x = print_endline (string_of_bool x);;
let print_float_endline x = print_endline (string_of_float x);;
let print_tuple_endline = function (a, b) ->
  print_int a; print_string " "; print_int b; print_endline "";;

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
