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
let breadthBT2 bt =
  let insert elem (xs, ys) = match elem with
    | Empty -> (xs, ys)
    | Node(v, bt1, bt2) -> (v :: xs, bt2 :: bt1 :: ys)
  in
  let rec iter = function
    | [] -> []
    | Empty::tl -> iter tl
    | xs -> match (List.fold_left (fun (acc, queue) elem -> insert elem (acc, queue)) ([], []) xs) with (acc, queue) -> List.rev acc @ iter (List.rev queue)
  in iter [bt]
;;

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
print_list_int (breadthBT2 tt);;

print_endline "## Zad 5";;

let depthSearch (Graph graph) startNode =
  let rec search visited = function
    | [] -> []
    | h::t -> if List.mem h visited then search visited t
    else h::search (h::visited) (graph h @ t)
  in search [] [startNode]
;;

(*
let breadthSearch (Graph graph) startNode =
  let rec search visited = function
    | [] -> []
    | h::t -> if List.mem h visited then search visited t
    else h::search (h::visited) (t @ graph h)
  in search [] [startNode]
;;
*)

let g = Graph
(function
    | 0 -> [3]
    | 1 -> [0;2;4]
    | 2 -> [1]
    | 3 -> []
    | 4 -> [0;2]
    | n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
);;

print_list_int (depthSearch g 4);;
(* print_list_int (breadthSearch g 4);; *)
