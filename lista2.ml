let print_list_string xs = print_endline (String.concat " " xs);;
let print_list_char xs = print_list_string (List.map (String.make 1) xs);;
let print_list_int xs = print_list_string (List.map string_of_int xs);;
let print_int_endline x = print_int x; print_endline "";;
let print_bool_endline x = print_endline (string_of_bool x);;
let print_float_endline x = print_endline (string_of_float x);;

print_endline "## Zad 2";;
let rec fib n =
  match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fib (n-2) + fib (n-1)
;;
let fibTail n =
  let rec fibIter(n, x1, x2) =
    if n<=1 then x2 else fibIter(n-1, x2, x1+x2)
  in fibIter (n, 0, 1)
;;

print_int_endline (fibTail 42);;
print_int_endline (fib 42);;

print_endline "## Zad 3";;
let root3 a =
  let rec root3Iter accum =
    if abs_float (accum*.accum*.accum -. a) <= 1e-15 *. abs_float a then accum
    else root3Iter (accum +. (a/.(accum*.accum) -. accum) /. 3.)
  in root3Iter(if a > 1. then a /. 3. else a)
;;
print_float_endline (root3 8.);;
print_float_endline (root3 7.);;

print_endline "## Zad 4";;
let [_;_;x1;_;_] = [-2;-1;0;1;2];;
let [_;(x2,_)] = [(1,2);(0,1)];;
print_int_endline x1;;
print_int_endline x2;;

print_endline "## Zad 5";;
let rec initSegment xs ys =
  match (xs, ys) with
    | ([], _) -> true
    | (_, []) -> false
    | (h1::t1, h2::t2) -> match h1 = h2 with
        | true -> initSegment t1 t2
        | false -> false
;;

print_bool_endline (initSegment [] [] = true);;
print_bool_endline (initSegment [5] [] = false);;
print_bool_endline (initSegment [] [1; 2; 3] = true);;
print_bool_endline (initSegment [1; 2] [1; 2; 3] = true);;
print_bool_endline (initSegment [2; 2] [1; 2; 3] = false);;
print_bool_endline (initSegment [1; 2; 3] [1; 2; 3] = true);;

print_endline "## Zad 6";;
let rec replaceNth (xs, n, x) =
  match (xs, n) with
    | (_::t1, 0) -> x :: t1
    | (h1::t1, _) -> h1 :: replaceNth (t1, n-1, x)
    | _ -> raise (Failure "Invalid arguments")
;;

print_list_char (replaceNth(['o';'l';'a';'m';'a';'k';'o';'t';'a'], 1, 's'));;
print_list_char (replaceNth([], 0, 's'));;
