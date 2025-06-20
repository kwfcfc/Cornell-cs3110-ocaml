open Tree

module type Map = sig
  type ('k, 'v) t
  (** [('k, 'v) t] is the type of maps that bind keys of type ['k] to values of
      type ['v]. *)

  val empty : ('k, 'v) t
  (** [empty] does not bind any keys. *)

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (** [insert k v m] is the map that binds [k] to [v], and also contains all the
      bindings of [m]. If [k] was already bound in [m], that old binding is
      superseded by the binding to [v] in the returned map. *)

  val lookup : 'k -> ('k, 'v) t -> 'v
  (** [lookup k m] is the value bound to [k] in [m]. Raises: [Not_found] if [k]
      is not bound in [m]. *)

  val bindings : ('k, 'v) t -> ('k * 'v) list
  (** [bindings m] is an association list containing the same bindings as [m].
      The keys in the list are guaranteed to be unique. *)
end

module AssocListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list
  (** The list [(k1, v1); ...; (kn, vn)] binds key [ki] to value [vi]. If a key
      appears more than once in the list, it is bound to the the left-most
      occurrence in the list. *)

  let empty = []
  let insert k v m = (k, v) :: m
  let lookup k m = List.assoc k m
  let keys m = List.(m |> map fst |> sort_uniq Stdlib.compare)
  let bindings m = m |> keys |> List.map (fun k -> (k, lookup k m))
end

module UniqAssocListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list
  (** The list [(k1, v1); ...; (kn, vn)] binds key [ki] to value [vi]. No
      duplicate keys may occur. *)

  let empty = []
  let insert k v m = (k, v) :: List.remove_assoc k m
  let lookup k m = List.assoc k m
  let bindings m = m
end

(* Exercise: binary search tree map *)
module BstMap : Map = struct
  type ('k, 'v) t = ('k, 'v) Bst.tree = Leaf | Node of ('k, 'v) Bst.node

  let empty = Bst.empty
  let insert k v m = Bst.insert (k, v) m

  let lookup k t =
    match Bst.find k t with
    | None -> raise Not_found
    | Some v -> v

  let bindings = Bst.to_list
end

(* Exercise: make char *)
module CharMap = Stdlib.Map.Make (Char)
(*
   key is the char type. empty : 'a t is the type of map that gives a default
   empty map. add : key -> 'a -> 'a t -> 'a t takes in a key, a value in 'a type
   and a map, and return a new map. remove: key -> 'a t -> 'a t takes in a key
   and a map, and return a new map with the key removed.
*)

(* Exercise: date order *)
type date = { month : int; day : int }

module Date = struct
  type t = date

  let compare =
   fun d1 d2 ->
    match compare d1.month d2.month with
    | 0 -> compare d1.day d2.day
    | c -> c
end

(* Exercise: calendar *)
module DateMap = Stdlib.Map.Make (Date)

type calendar = string DateMap.t
