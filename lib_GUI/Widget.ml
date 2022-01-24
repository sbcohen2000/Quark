type t = {
    measure :
      ?requested_width:int ->
      ?requested_height:int ->
      unit ->
      (int * int);
    paint : Mat2.t -> Rect.t -> unit;
    handler : Event.t -> dirty:bool -> bool
  }
;;

type label_state = {
    measured_size : int * int;
  }
;;

let create_label (face : TextPainter.font) (text : string) =
  let text_width, text_height = TextPainter.measure face text in
  let state = ref {
                  measured_size = 0, 0;
                } in
  let rec shrink (requested_width : int) (text : string) =
    let width, _ = TextPainter.measure face text in
    if width <= requested_width then text
    else let text' = String.sub text 0 (String.length text - 1) in
         shrink requested_width text' in
  let measure ?(requested_width  : int option) ?(requested_height : int option) () =
    match requested_width, requested_height with
    | Some width, Some height ->
       state := { measured_size = width, height };
       width, height
    | Some width, None ->
       state := { measured_size = width, text_height };
       width, text_height
    | None, Some height ->
       state := { measured_size = text_width, height };
       text_width, height
    | None, None ->
       state := { measured_size = text_width, text_height };
       text_width, text_height in
  let paint (view : Mat2.t) (clip : Rect.t) =
    let width, height = !state.measured_size in
    let text' = if width < text_width then shrink width text
                else text in
    let text_width, _ = TextPainter.measure face text' in
    let left_padding = Int.of_float (Float.floor (Float.of_int (width - text_width) /. 2.)) in
    let painter = TextPainter.create (left_padding, height / 2) face text' in
    TextPainter.paint view clip painter in
  let (label : t) = {
      measure;
      paint;
      handler = fun _ ~dirty -> dirty
    } in
  ref label
;;

type hover_state = Normal
                 | Hovered
                 | Pressed
;;

type button_state = {
    measured_size : int * int;
    hover_state : hover_state;
  }
;;

let create_button (face : TextPainter.font) (text : string) =
  let padding = 4 in
  let (normal_style : RectPainter.style) = {
      top_right_radius = 0;
      bottom_right_radius = 0;
      bottom_left_radius = 0;
      top_left_radius = 0;
      color = 0.22, 0.22, 0.22;
      border_color = Some (0.1, 0.1, 0.1)
    } in
  let (hovered_style : RectPainter.style) =
    { normal_style with color = 0.3, 0.3, 0.3 } in
  let (pressed_style : RectPainter.style) =
    let color = 0.38, 0.52, 0.85 in
    { normal_style with
      color;
      border_color = Some (0.38 *. 0.5,
                           0.52 *. 0.5,
                           0.85 *. 0.5) } in
  let (state : button_state ref) =
    ref {
        measured_size = 0, 0;
        hover_state = Normal;
      } in
  let get_current_style () =
    match !state.hover_state with
    | Normal -> normal_style
    | Hovered -> hovered_style
    | Pressed -> pressed_style in
  let set_hover_state (s : hover_state) =
    state := { !state with hover_state = s } in
  let label = create_label face text in
  let measure ?(requested_width  : int option) ?(requested_height : int option) () =
    match requested_width, requested_height with
    | Some width, Some height ->
       let desired_label_width = width - padding * 2 in
       let desired_label_height = height - padding * 2 in
       ignore (!label.measure ~requested_width:desired_label_width
                 ~requested_height:desired_label_height ());
       state := { !state with measured_size = width, height };
       width, height
    | Some width, None ->
       let desired_label_width = width - padding * 2 in
       let _, label_height = !label.measure ~requested_width:desired_label_width () in
       let size = width, (label_height + padding * 2) in
       state := { !state with measured_size = size }; size
    | None,       Some height ->
       let desired_label_height = height - padding * 2 in
       let label_width, _ = !label.measure ~requested_height:desired_label_height () in
       let size = (label_width + padding * 2), height in
       state := { !state with measured_size = size }; size
    | None, None ->
       let label_width, label_height = !label.measure () in
       let size = (label_width + padding * 2), (label_height + padding * 2) in
       state := { !state with measured_size = size }; size in
  let paint (view : Mat2.t) (clip : Rect.t) =
    let width, height = !state.measured_size in
    let rect = 0, 0, width, height in
    let painter = RectPainter.create [| rect |] in
    RectPainter.paint view clip (get_current_style ()) painter;
    let child_view = Mat2.translate view padding padding in
    !label.paint child_view clip in
  let handler (e : Event.t) ~(dirty : bool) =
    match e with
    | Event.Mouse_Down _ ->
       set_hover_state Pressed; true
    | Event.Mouse_Up _ ->
       set_hover_state Hovered; true
    | Event.Mouse_Enter ->
       set_hover_state Hovered; true
    | Event.Mouse_Leave ->
       set_hover_state Normal; true
    | _ -> dirty in
  let (button : t) = {
      measure;
      paint;
      handler;
    } in
  ref button
;;

exception TODO of string

(* holds the position of the upper right hand corner of
 * each child *)
type row_state = Rect.t list
;;

let create_row (children : t ref list) =
  let state = ref ([] : row_state) in
  let measure requested_width requested_height () =
    state := [];
    match requested_width, requested_height with
    | Some width, Some _ ->
       let rec layout (children : t ref list) (rows : Rect.t list) =
         (match children with
          | [] -> rows
          | child::rest ->
             let curr_row = List.hd rows in
             let _, y, w, h = curr_row in
             let child_width, child_height = !child.measure () in
             if width < w + child_width then (* make a new row *)
               let new_row = 0, y + h, child_width, child_height in
               state := (0, y + h, child_width, child_height)::!state;
               layout rest (new_row::rows)
             else (* add to the existing row *)
               let curr_row' = 0, y, w + child_width,
                               if child_height > h then child_height else h in
               state := (w, y, child_width, child_height)::!state;
               layout rest (curr_row'::(List.tl rows))) in
       let rows = layout children [(0, 0, 0, 0)] in
       state := List.rev !state; rows
    | _ -> raise (TODO "measure row with unspecified width or height") in
  let paint (view : Mat2.t) (clip : Rect.t)=
    List.iter2 (fun child (px, py, _, _) ->
        let child_view = Mat2.move view px py in
        !child.paint child_view clip;
      ) children !state in
  let handler (e : Event.t) ~(dirty : bool) =
    match e with
    | Event.Mouse_Down p | Event.Mouse_Up p ->
       let child_responded =
         List.exists2 (fun child rect ->
             if Rect.is_inside rect p then
               let e' = Event.translate_position
                          (- Rect.x rect) (- Rect.y rect) e in
               !child.handler e' ~dirty
             else false) children !state in
       if child_responded then true
       else dirty
    | Event.Mouse_Move (a, b) ->
       let child_responses =
         List.map2 (fun child rect ->
             let is_inside = Rect.is_inside rect in
             if is_inside a && not (is_inside b)
             then !child.handler Event.Mouse_Leave ~dirty
             else if is_inside b && not (is_inside a)
             then !child.handler Event.Mouse_Enter ~dirty
             else false) children !state in
       if List.exists ((=) true) child_responses then true
       else dirty
    | _ -> dirty in
  let (row : t) =
    {
      measure = (fun ?requested_width ?requested_height () ->
        let rows = measure requested_width requested_height () in
        let outline = List.fold_left (fun outline row -> Rect.union outline row)
                        (0, 0, 0, 0) rows in
        Rect.width outline, Rect.height outline);
      paint;
      handler;
    } in
  ref row
;;

(* state is rect outline of each child *)
type stack_state = {
    rects : Rect.t list;
    clip : Rect.t
  }
;;

let create_stack (items : (t ref * Point.t) list) =
  let state = ref ({ rects = []; clip = 0, 0, 0, 0 } : stack_state) in
  let rec layout (items : (t ref * Point.t) list) (outline : Rect.t) =
    (match items with
     | [] -> outline
     | (child, (px, py))::rest ->
        let child_width, child_height = !child.measure () in
        let child_rect = px, py, child_width, child_height in
        state := { !state with rects = child_rect::!state.rects };
        layout rest (Rect.union outline child_rect)) in
  let measure requested_width requested_height () =
    state := { rects = []; clip = 0, 0, 0, 0 };
    let max_outline = match requested_width, requested_height with
      | Some width, Some height -> 0, 0, width, height
      | Some width, None -> 0, 0, width, 0
      | None, Some height -> 0, 0, 0, height
      | None, None -> 0, 0, 0, 0 in
    let actual_outline = layout items max_outline in
    state := { rects = List.rev !state.rects;
               clip = Rect.union actual_outline max_outline };
    !state.clip in
  let paint (view : Mat2.t) (clip : Rect.t) =
    List.iter (fun (child, (px, py)) ->
        let child_view = Mat2.translate view px py in
        !child.paint child_view clip
      ) items in
  let handler (e : Event.t) ~(dirty : bool) =
    match e with
    | Event.Mouse_Down p | Event.Mouse_Up p ->
       let child_responded =
         List.exists2 (fun (child, _) rect ->
             if Rect.is_inside rect p then
               let e' = Event.translate_position
                          (- Rect.x rect) (- Rect.y rect) e in
               !child.handler e' ~dirty
             else false) (List.rev items) (List.rev !state.rects) in
       if child_responded then true
       else dirty
    | Event.Mouse_Move (a, b) ->
       let child_responses =
         List.map2 (fun (child, _) rect ->
             let is_inside = Rect.is_inside rect in
             if is_inside a && not (is_inside b)
             then !child.handler Event.Mouse_Leave ~dirty
             else if is_inside b && not (is_inside a)
             then !child.handler Event.Mouse_Enter ~dirty
             else false) items !state.rects in
       if List.exists ((=) true) child_responses then true
       else dirty
    | _ -> dirty in
  let (stack : t) = {
      measure = (fun ?requested_width ?requested_height () ->
        let outline = measure requested_width requested_height () in
        Rect.width outline, Rect.height outline);
      paint;
      handler;
    } in
  ref stack
;;

