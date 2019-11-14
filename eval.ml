open Printlib;;
type code = Elem of bool | Not | And | Or | Xor;;

let eval cxs =
  List.hd (List.fold_left (fun xs c -> 
    match c with
      | Not -> (match xs with h::t -> not h::t)
      | And -> (match xs with h1::h2::t -> (h1&&h2)::t)
      | Or -> (match xs with h1::h2::t -> (h1||h2)::t)
      | Xor -> (match xs with h1::h2::t -> not (h1&&h2)::t)
      | Elem x -> x::xs
  ) [] cxs)
;;

let instr1 = [Elem true; Elem false; And; Not];;
let instr2 = [Elem true; Elem false; And; And];;
let instr3 = [Elem true; Not];;
let instr4 = [Elem true; Elem false; Xor];;
let instr5 = [Elem false; Elem true; Elem true; Xor; Or];;

print_bool_endline (eval instr1);;
(* print_bool_endline (eval instr2);; *)
print_bool_endline (eval instr3);;
print_bool_endline (eval instr4);;
print_bool_endline (eval instr5);;
