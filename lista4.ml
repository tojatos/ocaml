open Printlib;;

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;
type 'a graph = Graph of ('a -> 'a list);;

print_endline "## Zad 2";;
let f x = raise (Failure "");;

print_endline "## Zad 3";;

let breadthBT bt =
  let rec iter = function
    | [] -> []
    | Empty::tl -> iter tl
    | Node(v, bt1, bt2)::tl -> v :: iter (tl @ [bt1] @ [bt2])
  in iter [bt]
;;
(* TODO: optimize using fold_left (calculate all in queue and execute with new queue created from old) *)

let tt = Node(1,
               Node(2,
                    Node(4,
                         Empty,
                         Empty
                        ),
                    Empty
                   ),
               Node(3,
                    Node(5,
                         Empty,
                         Node(6,
                              Empty,
                              Empty
                             )
                        ),
                    Empty
                   )
              );;
print_list_int (breadthBT tt);;
