type control_point = {
    point : Point.t;
    dir : Point.t;
    pre_extent : float;
    post_extent : float;
  }

type t = control_point list
type path = Point.t list

val create : t -> path

(* The set of vectors which are normal
 * to each segment in the path *)
val normals : path -> path

(* The set of vectors which bisect the
 * angle between adjacent line segments *)
val bisectors : path -> path

