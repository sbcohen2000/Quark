type t = float * float * float * float (* x, y, w, h *)

val x : t -> float
val y : t -> float
val width : t -> float
val height : t -> float

val is_inside : t -> Point.t -> bool
val union : t -> t -> t
