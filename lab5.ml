open Printlib;;

print_endline "## Zad 1";;

type ('a, 'b) ab = A of 'a | B of 'b;;
let lint = [1;2;3] and lfloat = [1.;5.4;-3.4];;
let lsint = B 1.3 :: (List.map (fun x -> A x) lint) @ (List.map (fun x -> B x) lfloat);;

let rozdziel xs =
  List.fold_right (fun x (a, b)-> match x with
    | A y -> (y::a, b)
    | B y -> (a, y::b)
  ) xs ([], [])
;;

rozdziel lsint;;

print_endline "## Zad 3";;

type pole = {nazwa: string; cena: float; ilosc: int};;

let lpole = [
  {nazwa = "Ziemniaki"; cena = 2.5; ilosc = 120};
(*   {nazwa = "Auto"; cena = 300.; ilosc = 0}; *)
  {nazwa = "Funkcja"; cena = 0.; ilosc = 6};
];;

let find pole = List.find (fun {nazwa=n; cena=c; ilosc=i} -> c > 100. && i = 0) pole;;

print_endline "## Zad 4";;
let name pole =
  try (
    match (find pole) with
     {nazwa=n} -> n
  )
  with Not_found -> "brak towaru";;

print_endline (name lpole);;
