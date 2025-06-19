module type BST_TYPE = sig
  type ('k, 'v) tree = Leaf | Node of ('k, 'v) node

  and ('k, 'v) node = {
    key : 'k;
    value : 'v;
    left : ('k, 'v) tree;
    right : ('k, 'v) tree;
  }

  val empty : ('k, 'v) tree
  (** [empty] does not bind any node. *)

  (* val compare : 'k -> 'k -> int *)
  (** [compare x y] is the same as Stdlib.compare function *)

  val find : 'k -> ('k, 'v) tree -> 'v option
  (** [find x t] is whether [x] is a key at some node in tree and return the
      value [t]. *)

  val insert : 'k * 'v -> ('k, 'v) tree -> ('k, 'v) tree
  (** [insert x t] adds a new node to the tree, the old node is superseded by
      the new one. *)

  val to_list : ('k, 'v) tree -> ('k * 'v) list
  (** [to_list t] is an association list containing the same key value pair as
      [t]. The keys in the list are guaranteed to be unique and sorted *)
end

module Bst : BST_TYPE = struct
  type ('k, 'v) tree = Leaf | Node of ('k, 'v) node

  and ('k, 'v) node = {
    key : 'k;
    value : 'v;
    left : ('k, 'v) tree;
    right : ('k, 'v) tree;
  }

  let empty = Leaf
  let compare = Stdlib.compare

  let rec find x = function
    | Leaf -> None
    | Node { key; value; left; right } -> (
      match compare x key with
      | 0 -> Some value
      | -1 -> find x left
      | 1 -> find x right
      | _ -> None)

  let rec insert x = function
    | Leaf -> Node { key = fst x; value = snd x; left = Leaf; right = Leaf }
    | Node { key; value; left; right } -> (
      match compare (fst x) key with
      | 0 -> Node { key = fst x; value = snd x; left; right }
      | -1 -> Node { key; value; left = insert x left; right }
      | 1 -> Node { key; value; left; right = insert x right }
      | _ -> Node { key; value; left; right })

  let rec to_list = function
    | Leaf -> []
    | Node { key; value; left = Leaf; right } -> (key, value) :: to_list right
    | Node { key; value; left; right = Leaf } -> to_list left @ [ (key, value) ]
    | Node { key; value; left; right } ->
      to_list left @ ((key, value) :: to_list right)
end
