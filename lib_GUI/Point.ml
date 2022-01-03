type t = float * float
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

let norm (a : t) =
  let s = mag a in
  scale a (1. /. s)
;;

let dot (a : t) (b : t) =
  let ax, ay = a
  and bx, by = b in
  ax *. bx +. ay *. by
;;
