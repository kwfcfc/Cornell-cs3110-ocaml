(* Exercise: complex synonym *)
module type ComplexSig = sig
  type t = float * float

  val add : t -> t -> t
  val zero : t
end

(* Exercise: complex encapsulation *)
module Complex : ComplexSig = struct
  type t = float * float

  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end

(*
   - remove zero from the structure: The value zero is required but not
     provided. Reason: the value zero is required in the signature that
     specifies the functions and variables needed in the module.

   - remove add from the signature: No error, but compiler throws an error for
     unused value add in the struct, and the client cannot use add function.

   - change zero in the structure to let zero = 0, 0: The value zero does not
     match because it requires float * float but int is not compatible with
     float.
*)

(* Exercise: big list queue *)

(** Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let open Queue in
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (ListQueue.enqueue n q)
  in
  loop n ListQueue.empty

(* 10, 100, and 1000 length in utop using following code:
   {[
     let t0 = Sys.time () in
     let _result = fill_listqueue 10000 in
     let t1 = Sys.time () in
     Printf.printf "Elapsed time: %f seconds\n" (t1 -. t0)
   ]}
   - 10: Elapsed time: 0.000007 seconds
   - 100: Elapsed time: 0.000111 seconds
   - 1000: Elapsed time: 0.010061 seconds
   - 10000: Elapsed time: 0.358835 seconds, this is when it creates a noticeable
     delay
   - 100000: Elapsed time: 87.854593 seconds, this is when the delay is greater
     than 10 seconds.
*)

(* Exercise: big batched queue *)
let fill_batchedqueue n =
  let open Queue in
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (BatchedQueue.enqueue n q)
  in
  loop n BatchedQueue.empty

(* 10, 100, and 1000 length in utop using following code:
   {[
     let t0 = Sys.time () in
     let _result = fill_batchedqueue 10 in
     let t1 = Sys.time () in
     Printf.printf "Elapsed time: %f seconds\n" (t1 -. t0)
   ]}
   - 10: Elapsed time: 0.000003 seconds
   - 100: Elapsed time: 0.000004 seconds
   - 1_000: Elapsed time: 0.000023 seconds
   - 10_000: Elapsed time: 0.000194 seconds
   - 100_000: Elapsed time: 0.005589 seconds
   - 1_000_000: Elapsed time: 0.041278 seconds
   - 10_000_000: Elapsed time: 0.278753 seconds
   - 50_000_000: Elapsed time: 1.332053 seconds
   - After 50_000_000, the process is killed because of OOM
*)

(* Exercise: queue efficiency *)

(*
   1. The efficiency of ListQueue.enqueue is linear time because the ( @ ) 
   operator will append new element at the end of the list, and it requires 
   traversing the whole length of the list and takes linear time. Therefore, 
   adding n elements will require traversing the list n times and each require 
   linear time, so the total complexity is quadratic.
   2. In BatchedQueue.enqueue where the queue never has any elemented dequeued, 
   the enqueue is just constructing the new element at the head of the inbox 
   list and it is constant time. Therefore, adding n elements to the queue with 
   each taking constant time gets the total time complexity to linear time.
*)

(* Exercise: binary search tree map *)
