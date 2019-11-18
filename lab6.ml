open Printlib;;

type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;
let rec ltake = function
  | (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, lazy xs)) -> x::ltake(n-1, xs)
;;

print_endline "## Zad 1";;

let lpow n =
  let rec lpowIter acc =
    LCons(acc, lazy(lpowIter (n*acc)))
  in lpowIter 1
;;

print_list_int (ltake (11, lpow 5));;
