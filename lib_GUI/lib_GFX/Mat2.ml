type t = {
    width : int;
    height : int;
    translate_x : int;
    translate_y : int;
  }
;;

type raw = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
;;

let ortho ~left ~right ~bottom ~top ~near ~far =
  let drl = 1. /. (right -. left) in
  let dtb = 1. /. (top -. bottom) in
  let dfn = 1. /. (far -. near) in
  let arr = [| (  2. *. drl) ; 0. ; 0. ; 0. ; (* column 1 *)
               0. ; (  2. *. dtb) ; 0. ; 0. ; (*        2 *)
               0. ; 0. ; (-.2. *. dfn) ; 0. ; (*        3 *)
               (-.(right +.   left) *. drl) ; (*        4 *)
               (-.(top   +. bottom) *. dtb) ; (*        : *)
               (-.(far   +.   near) *. dfn) ; 1. |] in
  Bigarray.(Array1.of_array Bigarray.float32
              Bigarray.c_layout arr)
;;

let export (mat : t) =
  let left   = Float.of_int (-mat.translate_x)
  and right  = Float.of_int (mat.width - mat.translate_x)
  and top    = Float.of_int (-mat.translate_y)
  and bottom = Float.of_int (mat.height - mat.translate_y) in
  ortho ~left ~right ~bottom ~top
    ~near: (-.1.)
    ~far: 1.
;;

let identity (width : int) (height : int) =
  ({ width; height; translate_x = 0; translate_y = 0; } : t)
;;

let set_translation (mat : t) (x : int) (y : int) =
  { mat with translate_x = x;
             translate_y = y; }
;;

let move (mat : t) (x : int) (y : int) =
  let width = mat.width
  and height = mat.height
  and translate_x = mat.translate_x + x
  and translate_y = mat.translate_y + y in
  ({ width; height; translate_x; translate_y })
;;

let apply_point (mat : t) (p : Point.t) =
  let px, py = p in
  px + mat.translate_x, py + mat.translate_y
;;

let apply_rect (mat : t) (r : Rect.t) =
  let x, y, w, h = r in
  x + mat.translate_x, y + mat.translate_y, w, h
;;
