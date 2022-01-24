type t = int * int * int * int
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
  && px <= x rect + width rect
  && py >= y rect
  && py <= y rect + height rect
;;

let union rect_a rect_b =
  let xs = min (x rect_a) (x rect_b) in
  let ys = min (y rect_a) (y rect_b) in
  let xe = max (x rect_a + width rect_a) (x rect_b + width rect_b) in
  let ye = max (y rect_a + height rect_a) (y rect_b + height rect_b) in
  xs, ys, (xe - xs), (ye - ys)
;;

let centered (rect : t) ~(width : int) ~(height : int) =
  let x, y, w, h = rect in
  let horz_padding = (w - width) / 2
  and vert_padding = (h - height) / 2 in
  x + horz_padding, y + vert_padding, width, height
;;

let to_float (rect : t) =
  let x, y, w, h = rect in
  Float.of_int x, Float.of_int y,
  Float.of_int w, Float.of_int h
;;
