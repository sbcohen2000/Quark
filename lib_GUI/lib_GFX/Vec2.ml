type t = float * float

let of_point (a : Point.t) =
  let x, y = a in
  Float.of_int x, Float.of_int y
;;

let to_point (a : t) =
  let x, y = a in
  Int.of_float (Float.round x),
  Int.of_float (Float.round y)
;;

let add (a : t) (b : t) =
  let ax, ay = a
  and bx, by = b in
  ax +. bx, ay +. by
;;

let sub (a : t) (b : t) =
  let ax, ay = a
  and bx, by = b in
  ax -. bx, ay -. by
;;

let neg (a : t) =
  let x, y = a in
  -.x, -.y
;;

let scale (a : t) (s : float) =
  let ax, ay = a in
  ax *. s, ay *. s
;;

let mag (a : t) =
  let ax, ay = a in
  Float.sqrt (Float.pow ax 2. +. Float.pow ay 2.)
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
  ax *. bx +. ay *. by
;;

let to_string (a : t) =
  let x, y = a in
  "(" ^ Float.to_string x ^ ", " ^ Float.to_string y ^ ")"
;;
