(* Author: Sam Westrick (samwestrick.com)
 * May 2020
 *)

functor ContTopDownSplay (Key: KEY) : SPLAY =
struct
  structure Key = Key

  type k = Key.t

  datatype t = Empty | Node of t * k * t

  (* can we do it with continuations?? pass in odd/even continuations *)

  fun left (Node (A, x, B)) = A

  fun right (Node (A, x, B)) = B

  fun zigzig (Node (Node (_, p, C), g, D)) (A, B) =
    (A, Node (B, p, Node (C, g, D)))

  fun zigzag (Node (Node (A, p, _), g, D)) (B, C) =
    (Node (A, p, B), Node (C, g, D))

  fun zagzig (Node (A, g, Node (_, p, D))) (B, C) =
    (Node (A, g, B), Node (C, p, D))

  fun zagzag (Node (A, g, Node (B, p, _))) (C, D) =
    (Node (Node (A, g, B), p, C), D)

  fun zig (Node (_, p, C)) (A, B) =
    (A, Node (B, p, C))

  fun zag (Node (A, p, _)) (B, C) =
    (Node (A, p, B), C)

  fun splayLeftLeft x t oneUp twoUp =
    case left (left t) of
      Empty => twoUp (zigzig t (Empty, Empty))
    | Node (L, k, R) =>
        case Key.compare (x, k) of
          EQUAL   => twoUp (zigzig t (L, R))
        | LESS    => splayLeftLeft  x (left t) (twoUp o zigzig t) oneUp
        | GREATER => splayLeftRight x (left t) (twoUp o zigzig t) oneUp

  and splayLeftRight x t oneUp twoUp =
    case right (left t) of
      Empty => twoUp (zigzag t (Empty, Empty))
    | Node (L, k, R) =>
        case Key.compare (x, k) of
          EQUAL   => twoUp (zigzag t (L, R))
        | LESS    => splayRightLeft  x (left t) (twoUp o zigzag t) oneUp
        | GREATER => splayRightRight x (left t) (twoUp o zigzag t) oneUp

  and splayRightLeft x t oneUp twoUp =
    case left (right t) of
      Empty => twoUp (zagzig t (Empty, Empty))
    | Node (L, k, R) =>
        case Key.compare (x, k) of
          EQUAL   => twoUp (zagzig t (L, R))
        | LESS    => splayLeftLeft   x (right t) (twoUp o zagzig t) oneUp
        | GREATER => splayLeftRight  x (right t) (twoUp o zagzig t) oneUp

  and splayRightRight x t oneUp twoUp =
    case right (right t) of
      Empty => twoUp (zagzag t (Empty, Empty))
    | Node (L, k, R) =>
        case Key.compare (x, k) of
          EQUAL   => twoUp (zagzag t (L, R))
        | LESS    => splayRightLeft  x (right t) (twoUp o zagzag t) oneUp
        | GREATER => splayRightRight x (right t) (twoUp o zagzag t) oneUp

  fun id x = x

  fun splayLeft x t =
    case left t of
      Empty => zig t (Empty, Empty)
    | Node (L, k, R) =>
        case Key.compare (x, k) of
          EQUAL   => zig t (L, R)
        | LESS    => splayLeftLeft  x t (zig t) id
        | GREATER => splayLeftRight x t (zig t) id

  fun splayRight x t =
    case right t of
      Empty => zag t (Empty, Empty)
    | Node (L, k, R) =>
        case Key.compare (x, k) of
          EQUAL   => zag t (L, R)
        | LESS    => splayRightLeft  x t (zag t) id
        | GREATER => splayRightRight x t (zag t) id

  fun splaySplit x t =
    case t of
      Empty => (Empty, Empty)
    | Node (L, k, R) =>
        case Key.compare (x, k) of
          EQUAL => (L, R)
        | LESS => splayLeft x t
        | GREATER => splayRight x t

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
