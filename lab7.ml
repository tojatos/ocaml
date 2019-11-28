open Printlib;;

print_endline "## Zad 1";;

let matrix m n =
  Array.init m (fun _ -> Array.make n 0);;

print_endline "## Zad 2";;

let mat = matrix 3 4;;

(*
let print_arr arr =
  print_endline (String.concat " " (List.map string_of_int (Array.to_list arr)));;

let print_matrix mat =
  Array.iter (fun arr -> print_arr arr) mat;;
*)

let print_matrix mat =
  let matMax = Array.length mat-1 in
  for i=0 to matMax do
    let arrMax = Array.length mat.(i)-1 in
    for j=0 to arrMax do
      print_int mat.(i).(j); print_string " "
    done;
    print_endline "";
  done;
  print_endline "";
;;

print_matrix mat;;

mat.(2).(0) <- 1;;
mat.(1).(2) <- 2;;

print_matrix mat;;

print_endline "## Zad 3";;

let firstify mat =
  let matMax = Array.length mat-1 in
  let arrMax = Array.length mat.(0)-1 in
  for i=0 to arrMax do
    mat.(0).(i) <- 1;
    mat.(matMax).(i) <- 1
  done;
  for i=0 to matMax do
    mat.(i).(0) <- 1;
    mat.(i).(arrMax) <- 1
  done;
;;

firstify mat;;
print_matrix mat;;

print_endline "## Zad 4";;

let lollies = [|5;6;6;10;4;5;2;9;3;4;5;3|];;
let days = [|2;3;7;20;8;3;3;6;1;6;3;6|];;

let zad4 lollies days =
  let loliLen = Array.length lollies in
  let loliMax = loliLen - 1 in
  let result = Array.make loliLen 0 in
  result.(loliMax) <- lollies.(loliMax);
  for i=(loliMax-1) downto 0 do
    let offset = days.(i) + i in
    let comp = lollies.(i) + (if offset > loliMax then 0 else result.(offset)) in
    result.(i) <- max result.(i+1) comp
  done;
  result
;;
print_list_int (Array.to_list (zad4 lollies days));;
