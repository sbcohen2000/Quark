type control_point = {
    point : Point.t;
    before : Point.t;
    after : Point.t;
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

