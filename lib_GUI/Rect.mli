type t = int * int * int * int (* x, y, w, h *)

val x : t -> int
val y : t -> int
val width : t -> int
val height : t -> int

val is_inside : t -> Point.t -> bool
val union : t -> t -> t
(* computes the rectangle having width and height
 * and is centered inside the input rect *)
val centered : t -> width:int -> height:int -> t

val to_float : t -> (float * float * float * float)
