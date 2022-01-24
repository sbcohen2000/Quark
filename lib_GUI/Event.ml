type t = Mouse_Up   of Point.t
       | Mouse_Down of Point.t
       | Mouse_Move of Point.t * Point.t
       | Mouse_Enter
       | Mouse_Leave
;;

let translate_position (x : int) (y : int) = function
  | Mouse_Up (x', y') -> Mouse_Up (x' + x, y' + y)
  | Mouse_Down (x', y') -> Mouse_Down (x' + x, y' + y)
  | Mouse_Move ((ax, ay), (bx, by)) ->
     Mouse_Move ((ax + x, ay + y), (bx + x, by + y))
  | e -> e
;;
