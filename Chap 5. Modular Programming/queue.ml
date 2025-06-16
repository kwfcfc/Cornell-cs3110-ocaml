module type Queue = sig
  type 'a t
  (** An ['a t] is a queue whose elements have type ['a]. *)

  exception Empty
  (** Raised if [front] or [dequeue] is applied to the empty queue. *)

  val empty : 'a t
  (** [empty] is the empty queue. *)

  val is_empty : 'a t -> bool
  (** [is_empty q] is whether [q] is empty. *)

  val enqueue : 'a -> 'a t -> 'a t
  (** [enqueue x q] is the queue [q] with [x] added to the end. *)

  val front : 'a t -> 'a
  (** [front q] is the element at the front of the queue. Raises [Empty] if [q]
      is empty. *)

  val dequeue : 'a t -> 'a t
  (** [dequeue q] is the queue containing all the elements of [q] except the
      front of [q]. Raises [Empty] is [q] is empty. *)

  val size : 'a t -> int
  (** [size q] is the number of elements in [q]. *)

  val to_list : 'a t -> 'a list
  (** [to_list q] is a list containing the elements of [q] in order from front
      to back. *)
end

module ListQueue : Queue = struct
  type 'a t = 'a list
  (** The list [x1; x2; ...; xn] represents the queue with [x1] at its front,
      followed by [x2], ..., followed by [xn]. *)

  exception Empty

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let enqueue x q = q @ [ x ]

  let front = function
    | [] -> raise Empty
    | x :: _ -> x

  let dequeue = function
    | [] -> raise Empty
    | _ :: q -> q

  let size = List.length
  let to_list = Fun.id
end
