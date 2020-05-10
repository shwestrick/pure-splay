(* Author: Sam Westrick (samwestrick.com)
 * May 2020
 *)

functor AwfulTopDownSplay (Key: KEY) (*: SPLAY *) =
struct
  structure Key = Key

  type k = Key.t

  datatype t = Empty | Node of t * k * t

  datatype result =
    Success of t * k * t
  | Failure of t * k * t

  (* This works but is so tedious. I don't want to implement
   * all the other cases *)
  fun splayLeftLeft x (((A, k, B), p, C), g, D) =
    case Key.compare (x, k) of
      EQUAL =>
        Success (A, x, Node (B, p, Node (C, g, D)))
    | LESS =>
        (case A of
          Empty => Success (A, k, Node (B, p, Node (C, g, D)))
        | Node stuffA =>
            (case splayLeftLeft x ((stuffA, k, B), p, C) of
              Success (L, y, R) => Failure (L, y, R)
            | Failure (L, y, R) => Success (L, y, Node (R, p, Node (C, g, D)))))
    | GREATER =>
        (case B of
          Empty => Success (A, k, Node (B, p, Node (C, g, D)))
        | Node stuffB =>
            (case splayLeftRight x ((A, k, stuffB), p, C) of
              Success (L, y, R) => Failure (L, y, R)
            | Failure (L, y, R) => Success (L, y, Node (R, p, Node (C, g, D)))))

  and splayLeftRight x ((A, p, (B, k, C)), g, D) =
    raise Fail "splayLeftRight not yet implemented"

end
