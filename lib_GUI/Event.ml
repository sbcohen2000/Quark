type t = Mouse_Up    of Point.t
       | Mouse_Down  of Point.t
       | Mouse_Move  of Point.t * Point.t
       | Mouse_Enter of Point.t (* second point *)
       | Mouse_Leave of Point.t (*  first point *)
;;

let translate_position (x : int) (y : int) = function
  | Mouse_Up (x', y') -> Mouse_Up (x' + x, y' + y)
  | Mouse_Down (x', y') -> Mouse_Down (x' + x, y' + y)
  | Mouse_Move ((ax, ay), (bx, by)) ->
     Mouse_Move ((ax + x, ay + y), (bx + x, by + y))
  | Mouse_Enter (x', y') -> Mouse_Enter (x' + x, y' + y)
  | Mouse_Leave (x', y') -> Mouse_Leave (x' + x, y' + y)
;;

let create_enter_or_leave (bounds : Rect.t) (e : t) =
  match e with
  | Mouse_Move (a, b) ->
     let is_inside = Rect.is_inside bounds in
     if is_inside a && not (is_inside b)
     then Some (Mouse_Leave a)
     else if is_inside b && not (is_inside a)
     then Some (Mouse_Enter b)
     else None
  | _ -> None
;;
