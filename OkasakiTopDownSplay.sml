(* Author: Sam Westrick (samwestrick.com)
 * May 2020
 *)

functor OkasakiTopDownSplay (Key: KEY): SPLAY =
struct
  structure Key = Key

  type k = Key.t

  datatype t = Empty | Node of t * k * t

  (* Inspired by Okasaki's splay heap partition. Returns two trees which
   * are the left and right subtree of k after splaying, as well as a boolean
   * indicating whether or not we found k. This is slightly different from
   * Okasaki's splay heap, because in his presentation, duplicate keys were
   * permitted, so the `split` function (or, as Okasaki calls it, `partition`)
   * can only return two trees where the left tree is just less-or-equal-to
   * the desired key.
   *
   * There are a few problems with this implementation. It is *not exactly*
   * equivalent to the original splay algorithm. This stems from two problems:
   *
   *   1. Odd/even length paths. If the path is even-length, then every step is
   *   either a zig-zag or a zag-zig or a zig-zig, etc. But if the path is
   *   odd-length, then the final step (right before returning the tree) will be
   *   either a single zig or single zag. In the case of an odd-length path,
   *   this implementation does the single zig/zag at the BOTTOM of the access
   *   path, rather than at the TOP.
   *
   *   2. When we don't find the element, we should instead splay the most
   *   recently accessed element to the root. Otherwise it will be impossible
   *   to implement `lookup` correctly.
   *)
  fun splaySplit x t =
    case t of
      Empty => (Empty, false, Empty)
    | Node (l, k, r) =>
        case Key.compare (x, k) of
          EQUAL => (l, true, r)
        | LESS => splayLeft x (l, k, r)
        | GREATER => splayRight x (l, k, r)

  and splayLeft x (l, g, r) =
    case l of
      Empty => (Empty, false, Node (Empty, g, r))
    | Node (ll, p, lr) =>
        case Key.compare (x, p) of
          EQUAL => (ll, true, Node (lr, g, r))
        | LESS =>
            (* zig-zig *)
            let val (lll, b, llr) = splaySplit x ll
            in (lll, b, Node (llr, p, Node (lr, g, r)))
            end
        | GREATER =>
            (* zig-zag *)
            let val (lrl, b, lrr) = splaySplit x lr
            in (Node (ll, p, lrl), b, Node (lrr, g, r))
            end

  and splayRight x (l, g, r) =
    case r of
      Empty => (Node (l, g, Empty), false, Empty)
    | Node (rl, p, rr) =>
        case Key.compare (x, p) of
          EQUAL => (Node (l, g, rl), true, rr)
        | LESS =>
            (* zag-zig *)
            let val (rll, b, rlr) = splaySplit x rl
            in (Node (l, g, rll), b, Node (rlr, p, rr))
            end
        | GREATER =>
            (* zag-zag *)
            let val (rrl, b, rrr) = splaySplit x rr
            in (Node (Node (l, g, rl), p, rrl), b, rrr)
            end

  fun insert k t =
    let val (l, _, r) = splaySplit k t
    in Node (l, k, r)
    end

  fun fromList keys =
    List.foldl (fn (k, t) => insert k t) Empty keys

  (* How do we do lookup?? We don't have access to the "most recently accessed"
   * key (before we found k or fell off the tree).
   *)
  fun lookup k t =
    raise Fail "OkasakiTopDownSplay.lookup"
    (* let val (l, b, r) = splaySplit k t
    in if b then
         (Node (l, k, r), true)
       else
         ???
    end *)
end
