open Printlib;;

type 'a nlist = Koniec | Element of 'a * ('a nlist);;
type 'a llist = LKoniec | LElement of 'a * (unit -> 'a llist);;

let rec from k = Element (k, from (k+1));;
let rec lfrom k = LElement (k, fun () -> lfrom (k+1));;
let rec take = function
  | (0, _) -> []
  | (_, Koniec) -> []
  | (n, Element(x, xs)) -> x::take(n-1, xs)
;;
let rec ltake = function
  | (0, _) -> []
  | (_, LKoniec) -> []
  | (n, LElement(x, xs)) -> x::ltake(n-1, xs())
;;

print_endline "## Zad 1";;

let l1 = Element(5, Element(6, Element(3, Element(2, Element(1, Koniec)))))
(* let l2 = LElement(5, LElement(6, LElement(3, LElement(2, LElement(1, LKoniec))))) *)

let podziel xs =
  let rec podzielIter = function
    | Element(head, tail) ->
        begin
          match tail with 
            | Element(_, t2) -> Element(head, podzielIter t2)
            | Koniec -> Element(head, Koniec)
        end
    | _ -> Koniec
  in begin
    match xs with 
      | Element(_, tail) -> (podzielIter xs, podzielIter tail)
      | Koniec -> (Koniec, Koniec)
  end
;;

let lpodziel xs =
  let rec lpodzielIter = function
    | LElement(head, tail) ->
        begin
          match tail() with 
            | LElement(_, t2) -> LElement(head, fun () -> lpodzielIter (t2()))
            | LKoniec -> LElement(head, fun () -> LKoniec)
        end
    | _ -> LKoniec
  in begin
    match xs with 
      | LElement(_, tail) -> (lpodzielIter xs, lpodzielIter (tail()))
      | LKoniec -> (LKoniec, LKoniec)
  end
;;

print_list_int (take (11, l1));;
let (x, y) = podziel l1;;
print_list_int (take (11, x));;
print_list_int (take (11, y));;
print_list_int (ltake (11, lfrom 5));;
let (lx, ly) = lpodziel (lfrom 5);;
print_list_int (ltake (11, lx));;
print_list_int (ltake (11, ly));;
