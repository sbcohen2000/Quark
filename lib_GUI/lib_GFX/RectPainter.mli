type t

type style = {
    top_right_radius    : int;
    bottom_right_radius : int;
    bottom_left_radius  : int;
    top_left_radius     : int;
    color               : (float * float * float);
    border_color        : (float * float * float) option;
  }

val create : Rect.t array -> t
val paint : Mat2.t -> Rect.t -> style -> t -> unit
