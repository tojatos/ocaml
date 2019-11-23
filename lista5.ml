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

let rec lrepeat k lxs =
  let rec lrepeatHelper = function
    | (_, LNil) -> LNil
    | (0, LCons(_, lazy tl)) -> lrepeatHelper (k, tl)
    | (a, (LCons(hd, _) as l)) -> LCons(hd, lazy(lrepeatHelper (a-1, l)))
  in lrepeatHelper (k, lxs)
;;

print_list_int (ltake (11, lrepeat 3 (lfrom 0)));;

print_endline "## Zad 2";;

let lfib =
  let rec lfibIter a b =
    LCons(a, lazy(lfibIter b (a+b)))
  in lfibIter 0 1
;;

print_list_int (ltake (11, lfib));;

print_endline "## Zad 3";;

let rec (@$) ll1 ll2 =
  match ll1 with
    | LNil -> ll2
    | LCons(x, lazy xf) -> LCons(x, lazy (xf @$ ll2))
;;


type 'a lBT = LEmpty | LNode of 'a * (unit -> 'a lBT) * (unit -> 'a lBT);;

let lBreadth bt =
  let rec iter = function
    | [] -> LNil
    | LEmpty::tl -> iter tl
    | LNode(v, bt1, bt2)::tl -> LCons(v, lazy(iter (tl @ [bt1(); bt2()])))
  in iter [bt]
;;

let rec lTree n = LNode(n, (fun () -> lTree (2*n)), fun () -> lTree (2*n+1));;

let e = (fun () -> LEmpty);;
let tree = LNode(1, (fun () -> LNode(4, e, e)), fun () -> LNode(7, e, e));;

print_list_int (ltake (21, lBreadth tree));;
print_list_int (ltake (71, lBreadth (lTree 0)));;
print_list_int (ltake (21, lBreadth (lTree 1)));;
print_list_int (ltake (21, lBreadth (lTree 2)));;
print_list_int (ltake (21, lBreadth (lTree 3)));;
