(* Author: Sam Westrick (samwestrick.com)
 * May 2020
 *)

functor ContTopDownSplay (Key: KEY) : SPLAY =
struct
  structure Key = Key

  type k = Key.t

  datatype t = Empty | Node of t * k * t

  (* can we do it with continuations?? pass in odd/even continuations *)

  fun splayLeftLeft x ((T, p, C), g, D) oneUp twoUp =
    let
      fun zigzig (A, B) = twoUp (A, Node (B, p, Node (C, g, D)))
    in
      case T of
        Empty => zigzig (Empty, Empty)
      | Node (A, k, B) =>
          case Key.compare (x, k) of
            EQUAL   => zigzig (A, B)
          | LESS    => splayLeftLeft  x ((A, k, B), p, C) zigzig oneUp
          | GREATER => splayLeftRight x ((A, k, B), p, C) zigzig oneUp
    end

  and splayLeftRight x ((A, p, T), g, D) oneUp twoUp =
    let
      fun zigzag (B, C) = twoUp (Node (A, p, B), Node (C, g, D))
    in
      case T of
        Empty => zigzag (Empty, Empty)
      | Node (B, k, C) =>
          case Key.compare (x, k) of
            EQUAL   => zigzag (B, C)
          | LESS    => splayRightLeft  x (A, p, (B, k, C)) zigzag oneUp
          | GREATER => splayRightRight x (A, p, (B, k, C)) zigzag oneUp
    end

  and splayRightLeft x (A, g, (T, p, D)) oneUp twoUp =
    let
      fun zagzig (B, C) = twoUp (Node (A, g, B), Node (C, p, D))
    in
      case T of
        Empty => zagzig (Empty, Empty)
      | Node (B, k, C) =>
          case Key.compare (x, k) of
            EQUAL   => zagzig (B, C)
          | LESS    => splayLeftLeft  x ((B, k, C), p, D) zagzig oneUp
          | GREATER => splayLeftRight x ((B, k, C), p, D) zagzig oneUp
    end

  and splayRightRight x (A, g, (B, p, T)) oneUp twoUp =
    let
      fun zagzag (C, D) = twoUp (Node (Node (A, g, B), p, C), D)
    in
      case T of
        Empty => zagzag (Empty, Empty)
      | Node (C, k, D) =>
          case Key.compare (x, k) of
            EQUAL   => zagzag (C, D)
          | LESS    => splayRightLeft  x (B, p, (C, k, D)) zagzag oneUp
          | GREATER => splayRightRight x (B, p, (C, k, D)) zagzag oneUp
    end

  fun id x = x

  fun splayLeft x (T, p, C) =
    let
      fun zig (A, B) = (A, Node (B, p, C))
    in
      case T of
        Empty => zig (Empty, Empty)
      | Node (A, k, B) =>
          case Key.compare (x, k) of
            EQUAL   => zig (A, B)
          | LESS    => splayLeftLeft  x ((A, k, B), p, C) zig id
          | GREATER => splayLeftRight x ((A, k, B), p, C) zig id
    end

  fun splayRight x (A, p, T) =
    let
      fun zag (B, C) = (Node (A, p, B), C)
    in
      case T of
        Empty => zag (Empty, Empty)
      | Node (B, k, C) =>
          case Key.compare (x, k) of
            EQUAL   => zag (B, C)
          | LESS    => splayRightLeft  x (A, p, (B, k, C)) zag id
          | GREATER => splayRightRight x (A, p, (B, k, C)) zag id
    end

  fun splaySplit x T =
    case T of
      Empty => (Empty, Empty)
    | Node (A, k, B) =>
        case Key.compare (x, k) of
          EQUAL => (A, B)
        | LESS => splayLeft x (A, k, B)
        | GREATER => splayRight x (A, k, B)

  fun insert x t =
    let
      val (l, r) = splaySplit x t
    in
      Node (l, x, r)
    end

  fun fromList keys =
    List.foldl (fn (k, t) => insert k t) Empty keys

  fun lookup x t =
    raise Fail "ContTopDownSplay.lookup not yet implemented"

end
