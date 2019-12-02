open Printlib;;

print_endline "## Zad 1"

module type POJ_FUN =
sig
  type 'a t
  exception Empty of string
  exception Full of string
  val create: unit -> 'a t
  val insert: 'a * 'a t -> 'a t
  val get: 'a t -> 'a * 'a t
end
;;

print_endline "## Zad 2"

module OptionPoj: POJ_FUN =
struct
  type 'a t = 'a option
  exception Empty of string
  exception Full of string
  let create() = None
  let insert(x, poj) = match poj with
    | Some x -> raise (Full "")
    | None -> Some x
  let get = function
    | Some x -> (x, None)
    | None -> raise (Empty "")
end
;;

let poj = OptionPoj.create();;
let poj2 = OptionPoj.insert(7, poj);;
(* let poj4 = OptionPoj.insert(5, poj2);; *)
let (x, poj3) = OptionPoj.get poj2;;
(* let (y, poj4) = OptionPoj.get poj3;; *)

print_int_endline x;;

print_endline "## Zad 3"

module ListPoj: POJ_FUN =
struct
  type 'a t = 'a list
  exception Empty of string
  exception Full of string
  let create() = []
  let insert(x, poj) = match poj with
    | [] -> [x]
    | _ -> raise (Full "")
  let get = function
    | [x] -> (x, [])
    | [] -> raise (Empty "")
    | _ -> failwith "implementation error"
end
;;

let poj = ListPoj.create();;
let poj2 = ListPoj.insert(7, poj);;
(* let poj4 = ListPoj.insert(5, poj2);; *)
let (x, poj3) = ListPoj.get poj2;;
(* let (y, poj4) = ListPoj.get poj3;; *)

print_int_endline x;;
