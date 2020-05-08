functor BottomUpSplay (Key: KEY): SPLAY =
struct
  structure Key = Key

  type k = Key.t

  datatype t = Empty | Node of t * k * t

  (* A bottom-up splay access (insert or lookup) operates in two phases.
   * First, we traverse the path to the desired node, and then we perform
   * rotations on the way back up, bringing the desired node to the root and
   * encouraging the accessed path to become more balanced. To implement this,
   * we'll write two functions: `path` and `splay`.
   *
   * Access paths are implemented by recording, at each step, whether the
   * next access is down-to-the-right or down-to-the-left. This produces a
   * list of "contexts", where each context is a parent node with a hole.
   *
   *   1. Right(L,k): the parent Node(L,k,_) is missing a right child.
   *
   *                                  k   <-- parent
   *                                 / \
   *                                L   * <-- you are here
   *
   *   2. Left(k,R): the parent Node(_,k,R) is missing a right child.
   *
   *                     parent -->   k
   *                                 / \
   *               you are here --> *   R
   *
   * By pushing elements onto the context list in the order they were accessed,
   * the "front" of the list is always the most recently accessed element.
   *
   * A "path" is then just a list of contexts together with the subtree whose
   * root is the targeted element. For example, consider searching for `y` in
   * the following tree.
   *
   *            z
   *           / \         Resulting path:
   *          x   D          ( subtree,     context list )
   *         / \           = ( Node(B,y,C), [Right(A,x), Left(z,D)] )
   *        A   y
   *           / \
   *          B   C
   *
   * With an access path in hand, we can splay it by performing the various
   * cases: zig-zig, zig-zag, etc. Each of the cases consumes two elements
   * off the end of the access path, except for the single zig/zag cases which
   * finish off an odd-length path.
   *)

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

  fun splay (l, k, r) anc =
    let val (l', r') = splay' (l, r) anc
    in Node (l', k, r')
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
