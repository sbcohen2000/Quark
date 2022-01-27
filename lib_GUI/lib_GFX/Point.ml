type t = int * int
;;

let add (a : t) (b : t) =
  let ax, ay = a
  and bx, by = b in
  ax + bx, ay + by
;;

let sub (a : t) (b : t) =
  let ax, ay = a
  and bx, by = b in
  ax - bx, ay - by
;;

let neg (a : t) =
  let x, y = a in
  -x, -y
;;

let scale (a : t) (s : float) =
  let ax, ay = a in
  let axf, ayf = Float.of_int ax, Float.of_int ay in
  Float.to_int (Float.round (axf *. s)),
  Float.to_int (Float.round (ayf *. s))
;;

let mag (a : t) =
  let ax, ay = a in
  let axf, ayf = Float.of_int ax, Float.of_int ay in
  Float.sqrt (Float.pow axf 2. +. Float.pow ayf 2.)
;;

let distance (a : t) (b : t) =
  mag (sub b a)
;;

let norm (a : t) =
  let s = mag a in
  scale a (1. /. s)
;;

let dot (a : t) (b : t) =
  let ax, ay = a
  and bx, by = b in
  ax * bx + ay * by
;;

let to_float (a : t) =
  let x, y = a in
  Float.of_int x, Float.of_int y
;;
