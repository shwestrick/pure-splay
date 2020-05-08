signature SPLAY =
sig
  structure Key: KEY

  type t
  type k = Key.t

  val insert: k -> t -> t

  (* lookups modify the tree too *)
  val lookup: k -> t -> (t * bool)

  (* build a tree by inserting keys in the given order *)
  val fromList: k list -> t
end
