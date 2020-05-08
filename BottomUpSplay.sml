(* Author: Sam Westrick (samwestrick.com)
 * May 2020
 *)

functor BottomUpSplay (Key: KEY): SPLAY =
struct
  structure Key = Key

  type k = Key.t

  datatype t = Empty | Node of t * k * t

  datatype context =
    Right of t * k
  | Left  of k * t

  type path = t * (context list)

  fun path k ((t, anc): path) =
    case t of
      Empty => (t, anc)
    | Node (L, x, R) =>
        case Key.compare (k, x) of
          LESS => path k (L, Left (x, R) :: anc)
        | EQUAL => (t, anc)
        | GREATER => path k (R, Right (L, x) :: anc)

  (* An explanation of what's going on in this function, splay':
   *
   *   - This is a helper function for implementing the primary splay function,
   *   further below. The primary splay function has a "center" key, x, to be
   *   put at the root. This center key is not named by splay', but it can be
   *   helpful to understand the algorithm if you imagine that it is there.
   *
   *   - splay' accumulates a tuple of trees (L, R) which will be the left and
   *   right subtrees of the implicit center key.
   *
   *   - Each case consumes either one or two parent nodes off the context list
   *   and performs rotations according to one of the zig/zag cases of the
   *   splay algorithm. This continues until we have consumed the entire
   *   ancestor context.
   *
   *   - Subtrees are named A, B, C, D to respect in-order traversal (i.e.
   *   notice that whenever we construct nodes, the names A, B, C, D always
   *   appear in order from left to right)
   *
   *   - The names `p` and `g` are used to refer to the parent and grandparent
   *   of the implicit center key. For example, in the zig-zag case, if `x` is
   *   the implicit center key, we perform the following transformation:
   *
   *             g                        x
   *            / \                     /   \
   *           p   D       ===>       p       g
   *          / \                    / \     / \
   *         A   x                  A   B   C   D
   *            / \
   *           B   C
   *)
  fun splay' (A, B) [] =
        (* done! *)
        (A, B)

    | splay' (A, B) [Left (p, C)] =
        (* zig *)
        (A, Node (B, p, C))

    | splay' (B, C) [Right (A, p)] =
        (* zag *)
        (Node (A, p, B), C)

    | splay' (A, B) (Left (p, C) :: Left (g, D) :: anc) =
        (* zig-zig *)
        splay' (A, Node (B, p, Node (C, g, D))) anc

    | splay' (C, D) (Right (B, p) :: Right (A, g) :: anc) =
        (* zag-zag *)
        splay' (Node (Node (A, g, B), p, C), D) anc

    | splay' (B, C) (Right (A, p) :: Left (g, D) :: anc) =
        (* zig-zag *)
        splay' (Node (A, p, B), Node (C, g, D)) anc

    | splay' (B, C) (Left (p, D) :: Right (A, g) :: anc) =
        (* zag-zig *)
        splay' (Node (A, g, B), Node (C, p, D)) anc

  (* Take the subtree Node(l,x,r) and splay x to the root by consuming the
   * ancestor context `anc`. The key x is the implicit center key for the
   * call to splay'. *)
  fun splay (l, x, r) anc =
    let val (l', r') = splay' (l, r) anc
    in Node (l', x, r')
    end

  fun insert k t =
    case path k (t, []) of
      (Node (l, _, r), anc) =>
        splay (l, k, r) anc
    | (Empty, anc) =>
        splay (Empty, k, Empty) anc

  (* Lookup is a little awkward, because we have to handle the case where
   * the node is not present, in which case we use the most recently accessed
   * node instead, but there might not be one if the tree was entirely empty.
   *)
  fun lookup k t =
    case path k (t, []) of
      (Node (l, _, r), anc) =>
        (splay (l, k, r) anc, true)

    | (Empty, []) =>
        (Empty, false)

    | (Empty, mostRecent :: anc) =>
        let
          val (l, x, r) =
            case mostRecent of
              Left (x, r) => (Empty, x, r)
            | Right (l, x) => (l, x, Empty)
        in
          (splay (l, x, r) anc, false)
        end

  fun fromList keys =
    List.foldl (fn (k, t) => insert k t) Empty keys
end
