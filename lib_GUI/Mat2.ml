(*
 * A = |a b|+|e|
 *     |c d| |f|
 * 
 * t = (a, b, c, d, e, f)
 *)

type t = (float * float * float * float * float * float)
;;

let make_pretty a b e c d f = a, b, c, d, e, f
;;

let identity =
  make_pretty
    1. 0.   0.
    0. 1.   0.
;;

let translation (x : float) (y : float) =
  make_pretty
    1. 0.   x
    0. 1.   y
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
  let a, b, c, d, e, f = mat in
  let arr = [| a ; c ; 0.; 0.;      (* column 1 *)
               b ; d ; 0.; 0.;      (*        2 *)
               0.; 0.; 1.; 0.;      (*        3 *)
               e ; f ; 0.; 1. |] in (*        4 *)
  Bigarray.(Array1.of_array Bigarray.float32
              Bigarray.c_layout arr)
;;
