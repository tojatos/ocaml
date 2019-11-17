open Printlib;;

type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let lhd = function
  | LNil -> failwith "lhd"
  | LCons (x, _) -> x
;;

let ltl = function
  | LNil -> failwith "ltl"
  | LCons (_, lazy t) -> t
;;

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;
let rec ltake = function
  | (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, lazy xs)) -> x::ltake(n-1, xs)
;;

(*
let rec lsquares = function
  | LNil -> LNil
  | LCons(x, lazy xs) -> LCons(x*x, lazy(lsquares xs))
;;

print_list_int (ltake (11, lsquares(lfrom 0)));;

let rec lmap f = function
  | LNil -> LNil
  | LCons(x, lazy xs) -> LCons(f x, lazy(lmap f xs))
;;

print_list_int (ltake (11, lmap (fun x -> x*x*x) (lfrom 0)));;
*)

print_endline "## Zad 1";;

let rec lrepeatIter (x, k, lxs) =
  if k = 0 then Lazy.force lxs
  else lrepeatIter (x, k-1, lazy(LCons(x, lxs)))
;;

let rec lrepeat k = function
  | LNil -> LNil
  | LCons(hd, lazy tl) -> lrepeatIter (hd, k, lazy(lrepeat k tl))
;;

print_list_int (ltake (11, lrepeat 3 (lfrom 0)));;

print_endline "## Zad 2";;

let lfib =
  let rec lfibIter a b =
    LCons(a, lazy(lfibIter b (a+b)))
  in
  lfibIter 0 1
;;

print_list_int (ltake (11, lfib));;
