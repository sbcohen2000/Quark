type t = float * float * float * float
;;

let x (v, _, _, _ : t) = v
;;
let y (_, v, _, _ : t) = v
;;
let width (_, _, v, _ : t) = v
;;
let height (_, _, _, v : t) = v
;;

let is_inside rect point =
  let px, py = point in
  px >= x rect
  && px <= x rect +. width rect
  && py >= y rect
  && py <= y rect +. height rect
;;

let union rect_a rect_b =
  let xs = Float.min (x rect_a) (x rect_b) in
  let ys = Float.min (y rect_a) (y rect_b) in
  let xe = Float.max (x rect_a +. width rect_a) (x rect_b +. width rect_b) in
  let ye = Float.max (y rect_a +. height rect_a) (y rect_b +. height rect_b) in
  xs, ys, (xe -. xs), (ye -. ys)
;;
