open Printlib;;

print_endline "## Zad 1"

module type QUEUE_FUN =
sig
  type 'a t
  exception Empty of string
  val empty: unit -> 'a t
  val enqueue: 'a * 'a t -> 'a t
  val dequeue: 'a t -> 'a t
  val first: 'a t -> 'a
  val isEmpty: 'a t -> bool
end
;;

module ListQueue: QUEUE_FUN =
struct
  type 'a t = 'a list
  exception Empty of string
  let empty() = []
  let enqueue(x, queue) = queue@[x]
  let dequeue = function
    | h::t -> t
    | _ -> raise (Empty "queue dequeue error")
  let first = function
    | h::t -> h
    | _ -> raise (Empty "queue first error")
  let isEmpty xs = xs = empty()
end
;;

module DoubleListQueue: QUEUE_FUN =
struct
  type 'a t = 'a list * 'a list
  exception Empty of string
  let empty() = ([], [])
  let enqueue(x, queue) = match queue with
    | ([], yl) -> (List.rev (x::yl), [])
    | (xl, yl) -> (xl, x::yl)
  let dequeue = function
    | (h::t, yl) -> begin match t with
        | [] -> (List.rev yl, [])
        | _ -> (t, yl)
      end
    | _ -> raise (Empty "queue dequeue error")
  let first = function
    | (h::t, _) -> h
    | _ -> raise (Empty "queue first error")
  let isEmpty xs = xs = empty()
end
;;

let queue = ListQueue.enqueue(7, ListQueue.enqueue(5, ListQueue.empty()));;
let queue2 = ListQueue.dequeue queue;;
let queue3 = ListQueue.dequeue queue2;;

let dqueue = DoubleListQueue.enqueue(7, DoubleListQueue.enqueue(5, DoubleListQueue.empty()));;
let dqueue2 = DoubleListQueue.dequeue dqueue;;
let dqueue3 = DoubleListQueue.dequeue dqueue2;;

print_int_endline (ListQueue.first queue);;
print_int_endline (ListQueue.first queue2);;
print_bool_endline (ListQueue.isEmpty queue2);;
print_bool_endline (ListQueue.isEmpty queue3);;
(*
print_int_endline (ListQueue.first queue3);;
*)

print_int_endline (DoubleListQueue.first dqueue);;
print_int_endline (DoubleListQueue.first dqueue2);;
print_bool_endline (DoubleListQueue.isEmpty dqueue2);;
print_bool_endline (DoubleListQueue.isEmpty dqueue3);;
(* print_int_endline (DoubleListQueue.first dqueue3);; *)

print_endline "## Zad 2"
module type QUEUE_MUT = 
sig 
  type 'a t 
        (* The type of queues containing elements of type ['a]. *) 
  exception Empty of string 
        (* Raised when [first q] is applied to an empty queue [q]. *) 
  exception Full of string 
        (* Raised when [enqueue(x,q)] is applied to a full queue [q]. *) 
  val empty: int -> 'a t 
        (* [empty n] returns a new queue of length [n], initially empty. *) 
  val enqueue: 'a * 'a t -> unit 
      (* [enqueue (x,q)] adds the element [x] at the end of a queue [q]. *) 
  val dequeue: 'a t -> unit 
        (* [dequeue q] removes the first element in queue [q] *)         
  val first: 'a t -> 'a 
       (* [first q] returns the first element in queue [q] without removing   
           it from the queue, or raises [Empty] if the queue is empty. *)  
  val isEmpty: 'a t -> bool 
        (* [isEmpty q] returns [true] if queue [q] is empty,  
           otherwise returns [false]. *) 
  val isFull: 'a t -> bool 
        (* [isFull q] returns [true] if queue [q] is full,  
           otherwise returns [false]. *) 
end
;; 

module QueueMut: QUEUE_MUT =
struct
  type 'a t = {a: 'a option array; mutable f: int; mutable r: int}
  exception Empty of string 
  exception Full of string 
  let empty n = {a = Array.make (n+1) None; f = 0; r = 0}
  let isEmpty q = q.r = q.f
  let isFull q = if q.r+1 = Array.length q.a then q.f = 0 else q.r+1 = q.f
  let enqueue(x, q) = if isFull q then raise (Full "enq")
    else begin
      q.a.(q.r) <- Some x;
      q.r <- q.r + 1;
      if q.r = Array.length q.a then q.r <- 0
    end
  let dequeue q = if isEmpty q then ()
    else begin
      q.f <- q.f + 1;
      if q.f = Array.length q.a then q.f <- 0
    end
  let first q = if isEmpty q then raise (Empty "first")
    else match q.a.(q.f) with
      | Some e -> e
      | None -> raise (Failure "first - impl error")
end
;;

let qumut = QueueMut.empty 2;;
print_bool_endline (QueueMut.isEmpty qumut);;
print_bool_endline (QueueMut.isFull qumut);;
QueueMut.enqueue(2, qumut);;
print_bool_endline (QueueMut.isEmpty qumut);;
print_bool_endline (QueueMut.isFull qumut);;
print_int_endline (QueueMut.first qumut);;
QueueMut.enqueue(3, qumut);;
print_bool_endline (QueueMut.isEmpty qumut);;
print_bool_endline (QueueMut.isFull qumut);;
print_int_endline (QueueMut.first qumut);;
QueueMut.dequeue qumut;;
print_int_endline (QueueMut.first qumut);;
QueueMut.dequeue qumut;;
print_bool_endline (QueueMut.isEmpty qumut);;
print_bool_endline (QueueMut.isFull qumut);;
