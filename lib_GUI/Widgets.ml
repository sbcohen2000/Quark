open GFX
exception TODO of string

module List = struct
  include List
  
  exception Unequal_Lengths
  let rec unzip = function
    | ((a, b)::rest) -> let a_rest, b_rest = unzip rest in
                        a::a_rest, b::b_rest
    | [] -> ([], [])
  ;;

  let zip = List.map2 (fun a b -> a, b)
  ;;
end
;;

type context = {
    mutable content_scale : float
  }
;;

open List

class virtual widget (ctx : context) (id : string option) =
        object
          val id = match id with Some s -> s | None -> ""
          method get_id = id
          method private scale (v : int) =
            Float.to_int (Float.round (Float.of_int v *. ctx.content_scale))
          
          method virtual measure : ?requested_width:int ->
                                   ?requested_height:int ->
                                   unit -> int * int
          method virtual paint : Mat2.t -> Rect.t -> unit
          method virtual handle : Event.t -> dirty:bool -> bool
          method virtual location_of_child : string -> Point.t -> Point.t option

        end
;;

class virtual container (ctx : context) (id : string option) =
        object(self)
          inherit widget ctx id

          method virtual private get_child_rects : unit -> (widget * Rect.t) list
          
          method handle (e : Event.t) ~(dirty : bool) =
            let child_widgets, child_rects = unzip (self#get_child_rects ()) in
            match e with
            | Event.Mouse_Down p | Event.Mouse_Up p ->
               let child_responded =
                 List.exists2 (fun child rect ->
                     if Rect.is_inside rect p then
                       let e' = Event.translate_position
                                  (- Rect.x rect) (- Rect.y rect) e in
                       child#handle e' ~dirty
                     else false) child_widgets child_rects in
               if child_responded then true
               else dirty
            | Event.Mouse_Move (a, b) ->
               let child_responses =
                 List.map2 (fun child rect ->
                     (* generate mouse enter / leave events *)
                     let r1 = match Event.create_enter_or_leave rect e with
                       | Some e' ->
                          let e'' = Event.translate_position
                                      (- Rect.x rect) (- Rect.y rect) e' in
                          child#handle e'' ~dirty
                       | None -> false in
                     (* forward mouse move events to children *)
                     let r2 = if Rect.is_inside rect a &&
                                   Rect.is_inside rect b then
                                let e' = Event.translate_position
                                           (- Rect.x rect) (- Rect.y rect) e in
                                child#handle e' ~dirty
                              else false in
                     r1 || r2 || dirty
                   ) child_widgets child_rects in
               List.exists ((=) true) child_responses || dirty
            | Event.Mouse_Enter p | Event.Mouse_Leave p ->
               let child_responses =
                 List.map2 (fun child rect ->
                     if Rect.is_inside rect p then
                       let e' = Event.translate_position
                                  (- Rect.x rect) (- Rect.y rect) e in
                       child#handle e' ~dirty
                     else false)
                   child_widgets child_rects in
               List.exists ((=) true) child_responses || dirty
          
          method location_of_child (child_id : string) (p : Point.t) =
            if child_id = id then Some p
            else let child_widgets, child_rects = unzip (self#get_child_rects ()) in
                 List.fold_left2 (fun location child rect ->
                     let child_position = Rect.x rect, Rect.y rect in
                     match location, child#location_of_child child_id
                                       (Point.add p child_position) with
                     | None, Some pos -> Some pos
                     | Some pos, _ -> Some pos
                     | _ -> None) None child_widgets child_rects
        end
;;

class spacer (ctx : context) (width : int) (height : int) =
  object
    inherit widget ctx None

    method measure ?(requested_width  : int option)
             ?(requested_height : int option) () =
      match requested_width, requested_height with
      | Some r_width, Some r_height -> r_width, r_height
      | Some r_width, None -> r_width, height
      | None, Some r_height -> width, r_height
      | None, None -> width, height

    method paint _ _ = ()
    method handle _ ~dirty = dirty
    method location_of_child (child_id : string) (p : Point.t) =
      if child_id = id then Some p else None
  end
;;

class label (ctx : context) (face : TextPainter.font) (text : string) =
  let text_width, text_height = TextPainter.measure face text in
  let rec shrink (requested_width : int) (text : string) =
    let width, _ = TextPainter.measure face text in
    if width <= requested_width then text
    else let text' = String.sub text 0 (String.length text - 1) in
         shrink requested_width text' in
  object
    inherit widget ctx None

    val mutable measured_size = 0, 0
    
    method measure ?(requested_width  : int option)
             ?(requested_height : int option) () =
      let size = match requested_width, requested_height with
        | Some width, Some height ->
           width, height
        | Some width, None ->
           width, text_height
        | None, Some height ->
           text_width, height
        | None, None ->
           text_width, text_height in
      (measured_size <- size;
       measured_size)

    method paint (view : Mat2.t) (clip : Rect.t) =
      let width, height = measured_size in
      let text' = if width < text_width then shrink width text
                  else text in
      let text_width, _ = TextPainter.measure face text' in
      let left_padding = Int.of_float (Float.floor (Float.of_int (width - text_width) /. 2.)) in
      let painter = TextPainter.create (left_padding, height / 2) face text' in
      TextPainter.paint view clip painter

    method handle _ ~dirty = dirty

    method location_of_child (child_id : string) (p : Point.t) =
      if child_id = id then Some p
      else None
  end
;;

type hover_state =
  | Normal
  | Hovered
  | Pressed
;;

class button (ctx : context) (face : TextPainter.font) (text : string) =
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
  let child = new label ctx face text in
  object(self)
    inherit widget ctx None
    
    val mutable measured_size = 0, 0
    val mutable hover_state = Normal

    method private current_style =
      match hover_state with
      | Normal -> normal_style
      | Hovered -> hovered_style
      | Pressed -> pressed_style
    
    method measure ?(requested_width  : int option) ?(requested_height : int option) () =
      let padding = self#scale padding in
      match requested_width, requested_height with
      | Some width, Some height ->
         let desired_label_width = width - padding * 2 in
         let desired_label_height = height - padding * 2 in
         ignore (child#measure ~requested_width:desired_label_width
                   ~requested_height:desired_label_height ());
         measured_size <- width, height;
         width, height
      | Some width, None ->
         let desired_label_width = width - padding * 2 in
         let _, label_height = child#measure ~requested_width:desired_label_width () in
         let size = width, (label_height + padding * 2) in
         measured_size <- size; size
      | None,       Some height ->
         let desired_label_height = height - padding * 2 in
         let label_width, _ = child#measure ~requested_height:desired_label_height () in
         let size = (label_width + padding * 2), height in
         measured_size <- size; size
      | None, None ->
         let label_width, label_height = child#measure () in
         let size = (label_width + padding * 2), (label_height + padding * 2) in
         measured_size <- size; size

    method paint (view : Mat2.t) (clip : Rect.t) =
      let padding = self#scale padding in
      let width, height = measured_size in
      let rect = 0, 0, width, height in
      let painter = RectPainter.create [| rect |] in
      RectPainter.paint view clip self#current_style painter;
      let child_view = Mat2.move view padding padding in
      child#paint child_view clip

    method handle (e : Event.t) ~(dirty : bool) =
      match e with
      | Event.Mouse_Down _ ->
         hover_state <- Pressed; true
      | Event.Mouse_Up _ ->
         hover_state <- Hovered; true
      | Event.Mouse_Enter _ ->
         hover_state <- Hovered; true
      | Event.Mouse_Leave _ ->
         hover_state <- Normal; true
      | _ -> dirty

    method location_of_child (child_id : string) (p : Point.t) =
      if child_id = id then Some p
      else if child_id = child#get_id then Some p
      else None
  end
;;

class row (ctx : context) (children : widget list) =
  object
    inherit container ctx None
    
    val mutable child_rects = ([] : Rect.t list)

    method get_child_rects () = List.zip children child_rects
    
    method measure ?(requested_width  : int option) ?(requested_height : int option) () =
      child_rects <- [];
      let rows =
        match requested_width, requested_height with
        | Some width, Some _ ->
           let rec layout (children : widget list) (rows : Rect.t list) =
             (match children with
              | [] -> rows
              | child::rest ->
                 let curr_row = List.hd rows in
                 let _, y, w, h = curr_row in
                 let child_width, child_height = child#measure () in
                 if width < w + child_width then (* make a new row *)
                   let new_row = 0, y + h, child_width, child_height in
                   child_rects <- (0, y + h, child_width, child_height)::child_rects;
                   layout rest (new_row::rows)
                 else (* add to the existing row *)
                   let curr_row' = 0, y, w + child_width,
                                   if child_height > h then child_height else h in
                   child_rects <- (w, y, child_width, child_height)::child_rects;
                   layout rest (curr_row'::(List.tl rows))) in
           let rows = layout children [(0, 0, 0, 0)] in
           child_rects <- List.rev child_rects; rows
        | None, None ->
           let width, height =
             List.fold_left (fun (width, height) (child : widget) ->
                 let child_width, child_height = child#measure () in
                 child_rects <- (width, 0, child_width, child_height)::child_rects;
                 width + child_width, max height child_height)
               (0, 0) children in
           let row = 0, 0, width, height in
           child_rects <- List.rev child_rects; [row]
        | _ -> raise (TODO "measure row with unspecified width or height") in
      let outline = List.fold_left Rect.union (0, 0, 0, 0) rows in
      Rect.width outline, Rect.height outline

    method paint (view : Mat2.t) (clip : Rect.t) =
      List.iter2 (fun child (px, py, _, _) ->
          let child_view = Mat2.move view px py in
          child#paint child_view clip;
        ) children child_rects
    
  end
;;

class column (ctx : context) (children : widget list) =
  object
    inherit container ctx None
    
    val mutable child_rects = ([] : Rect.t list)

    method get_child_rects () = List.zip children child_rects
    
    method measure ?(requested_width  : int option) ?(requested_height : int option) () =
      child_rects <- [];
      let rows =
        match requested_width, requested_height with
        | Some _, Some height ->
           let rec layout (children : widget list) (columns : Rect.t list) =
             (match children with
              | [] -> columns
              | child::rest ->
                 let curr_col = List.hd columns in
                 let x, _, w, h = curr_col in
                 let child_width, child_height = child#measure () in
                 if height < h + child_height then (* make a new row *)
                   let new_col = x + w, 0, child_width, child_height in
                   child_rects <- (x + w, 0, child_width, child_height)::child_rects;
                   layout rest (new_col::columns)
                 else (* add to the existing row *)
                   let curr_row' = x, 0, (if child_width > w then child_width else w),
                                   h + child_height in
                   child_rects <- (x, h, child_width, child_height)::child_rects;
                   layout rest (curr_row'::(List.tl columns))) in
           let rows = layout children [(0, 0, 0, 0)] in
           child_rects <- List.rev child_rects; rows
        | None, None ->
           let width, height =
             List.fold_left (fun (width, height) (child : widget) ->
                 let child_width, child_height = child#measure () in
                 child_rects <- (0, height, child_width, child_height)::child_rects;
                 max width child_width, height + child_height)
               (0, 0) children in
           let row = 0, 0, width, height in
           child_rects <- List.rev child_rects; [row]
        | _ -> raise (TODO "measure row with unspecified width or height") in
      let outline = List.fold_left Rect.union (0, 0, 0, 0) rows in
      Rect.width outline, Rect.height outline

    method paint (view : Mat2.t) (clip : Rect.t) =
      List.iter2 (fun child (px, py, _, _) ->
          let child_view = Mat2.move view px py in
          child#paint child_view clip;
        ) children child_rects
    
  end
;;


class stack (ctx : context) (children : (widget * Point.t) list) =
  let rec layout = function
    | [] -> []
    | (child, (px, py) : widget * Point.t)::rest ->
       let child_width, child_height = child#measure () in
       let child_rect = px, py, child_width, child_height in
       child_rect::(layout rest) in
  object
    inherit container ctx None
    
    val mutable child_rects = ([] : Rect.t list)
    val mutable clip_rect = 0, 0, 0, 0

    method get_child_rects () = List.map2 (fun (child, _) rect -> child, rect)
                                  children child_rects
    
    method measure ?(requested_width  : int option) ?(requested_height : int option) () =
      let max_outline = match requested_width, requested_height with
        | Some width, Some height -> 0, 0, width, height
        | Some width, None -> 0, 0, width, 0
        | None, Some height -> 0, 0, 0, height
        | None, None -> 0, 0, 0, 0 in
      child_rects <- layout children;
      clip_rect <- List.fold_left Rect.union max_outline child_rects;
      Rect.width clip_rect, Rect.height clip_rect

    method paint (view : Mat2.t) (clip : Rect.t) =
      List.iter (fun (child, (px, py)) ->
          let child_view = Mat2.move view px py in
          child#paint child_view clip
        ) children

  end
;;

class frame (ctx : context) (child : widget) ~(on_mouse_down : Point.t -> unit) =
  let title_height = 10 in
  let (title_style : RectPainter.style) =
    {
      top_right_radius = 0;
      bottom_right_radius = 0;
      bottom_left_radius = 0;
      top_left_radius = 0;
      color = 0.5, 0.3, 0.1;
      border_color = Some (1., 0.6, 0.2);
    } in
  object(self)
    inherit container ctx None as super
    
    val mutable child_rect = 0, 0, 0, 0

    method get_child_rects () = [child, child_rect]
    
    method measure ?(requested_width  : int option) ?(requested_height : int option) () =
      let title_height = self#scale title_height in
      let child_width, child_height = match requested_width, requested_height with
        | Some width, Some height ->
           child#measure ~requested_width:width
             ~requested_height:(height - title_height) ()
        | Some width, None ->
           child#measure ~requested_width:width ()
        | None, Some height ->
           child#measure ~requested_height:(height - title_height) ()
        | None, None ->
           child#measure () in
      child_rect <- 0, title_height, child_width, child_height;
      child_width, child_height + title_height

    method paint (view : Mat2.t) (clip : Rect.t) =
      let title_height = self#scale title_height in
      let title_rect = 0, 0, Rect.width child_rect, title_height in
      let painter = RectPainter.create [| title_rect |] in
      RectPainter.paint view clip title_style painter;
      let content_view = Mat2.move view 0 title_height in
      child#paint content_view clip

    method! handle (e : Event.t) ~(dirty : bool) =
      let title_height = self#scale title_height in
      let title_bar_rect = 0, 0, Rect.width child_rect, title_height in
      match e with
      | Event.Mouse_Down p when Rect.is_inside title_bar_rect p ->
         on_mouse_down p; true
      | _ -> super#handle e ~dirty
  end
;;

class receptacle (ctx : context) (id : string)
        ~(on_mouse_down : string -> unit)
        ~(on_mouse_up : string -> unit) =
  let size = 10 in
  let (style : RectPainter.style) = {
      top_right_radius = size / 2;
      bottom_right_radius = size / 2;
      bottom_left_radius = size / 2;
      top_left_radius = size / 2;
      color = 0.8, 0.8, 0.8;
      border_color = Some (0.5, 0.5, 0.5);
    } in
  object(self)
    inherit widget ctx (Some id)

    val mutable measured_size = 0, 0
    
    method measure ?(requested_width : int option) ?(requested_height : int option) () =
      let size = match requested_width, requested_height with
        | Some width, Some height -> width, height
        | Some width, None -> width, (self#scale size)
        | None, Some height -> (self#scale size), height
        | None, None -> (self#scale size), (self#scale size) in
      measured_size <- size; size

    method paint (view : Mat2.t) (clip : Rect.t) =
      let size = self#scale size in
      let rect = 0, 0, size, size in
      let mx, my = measured_size in
      let padding_x = (mx - size) / 2 in
      let padding_y = (my - size) / 2 in
      let painter = RectPainter.create [| rect |] in
      let view' = Mat2.move view padding_x padding_y in
      RectPainter.paint view' clip style painter

    method handle (e : Event.t) ~(dirty : bool) =
      match e with
      | Event.Mouse_Down _ -> on_mouse_down self#get_id; dirty
      | Event.Mouse_Up _ -> on_mouse_up self#get_id; dirty
      | _ -> dirty

    method location_of_child (child_id : string) (px, py : Point.t) =
      if child_id = id then Some (px + size / 2, py + size / 2)
      else None

  end
;;

class component
        (ctx : context)
        (face : TextPainter.font)
        ~(inputs : string list)
        ~(outputs : string list)
        ~(on_clicked_receptacle : string -> unit)
        ~(on_released_receptacle : string -> unit) =
  object(self)
    inherit widget ctx None

    val mutable child = new spacer ctx 0 0

    method private make_child () =
      let make_input_row (input : string) =
        (new row ctx [
             new receptacle ctx
               input
               ~on_mouse_down:(fun id -> on_clicked_receptacle id)
               ~on_mouse_up:(fun id -> on_released_receptacle id);
             new label ctx face input
           ] :> widget) in
      let make_output_row (output : string) =
        (new row ctx [
             new label ctx face output;
             new receptacle ctx
               output
               ~on_mouse_down:(fun id -> on_clicked_receptacle id)
               ~on_mouse_up:(fun id -> on_released_receptacle id)
           ] :> widget) in
      let make_input_col (inputs : string list) =
        new column ctx (List.map make_input_row inputs) in
      let make_output_col (outputs : string list) =
        new column ctx (List.map make_output_row outputs) in
      child <- (new row ctx [
                    ((make_input_col inputs) :> widget);
                    new spacer ctx 10 0;
                    ((make_output_col outputs) :> widget)
                  ] :> widget)

    initializer
      self#make_child ()
    
    method measure = child#measure
    method paint = child#paint
    method handle = child#handle

    method location_of_child (child_id : string) (p : Point.t) =
      if child_id = id then Some p
      else child#location_of_child child_id p
    
  end
;;

type drag_state =
  | NoDrag
  | Dragging_Frame of int * Point.t
  | Dragging_Wire of {
      start_port : string;
      current_location : Point.t
    }
;;

type component_spec =
  {
    inputs : string list;
    outputs : string list
  }
;;

class component_graph (ctx : context) (face : TextPainter.font) (components : component_spec list) =
  object(self)
    inherit widget ctx None

    val frame_positions = Array.init (List.length components) (fun _ -> 0, 0)
    val mutable wires = ([] : (string * string) list)
    val mutable dragging = NoDrag
    val mutable stack = new stack ctx []

    method private on_frame_mouse_down (frame_no : int) (offset : Point.t) =
      dragging <- Dragging_Frame (frame_no, offset)

    method private build_component (spec : component_spec) =
      new component ctx face ~inputs:(spec.inputs) ~outputs:(spec.outputs)
        ~on_clicked_receptacle:(fun start_port ->
          let drag = Dragging_Wire {
                         start_port;
                         current_location = 0, 0
                       } in
          dragging <- drag)
        ~on_released_receptacle:(fun end_port ->
          match dragging with
          | Dragging_Wire drag ->
             print_endline ("Made new wire from " ^ drag.start_port
                            ^ " to " ^ end_port);
             wires <- (drag.start_port, end_port)::wires;
             dragging <- NoDrag;
          | _ -> ())
    
    method private rebuild_stack () =
      let components = List.map self#build_component components in
      new stack ctx (List.mapi (fun (idx : int) (content : component) ->
                         ((new frame ctx (content :> widget)
                             ~on_mouse_down:(self#on_frame_mouse_down idx)) :> widget),
                         Array.get frame_positions idx) components)

    initializer
      stack <- self#rebuild_stack ()

    method measure = stack#measure
    
    method paint (view : Mat2.t) (clip : Rect.t) =
      begin
        (match dragging with (* draw wire preview *)
         | Dragging_Wire drag ->
            let start_location = stack#location_of_child drag.start_port (0, 0) in
            (match start_location with
             | Some start_location ->
                let x_distance = Int.abs (fst drag.current_location - fst start_location) in
                let control_point_length = min (x_distance / 2) 100 in
                let (curve : Curve.control_point list) =
                  [
                    { point = start_location;
                      before = 0, 0;
                      after = Point.add (control_point_length, 0) start_location };
                    { point = drag.current_location;
                      before = Point.add (-control_point_length, 0) drag.current_location;
                      after = 0, 0; }
                  ] in
                let painter = CurvePainter.create curve in
                CurvePainter.paint view clip painter;
             | _ -> ())
         | _ -> ());
        List.iter (fun wire -> (* draw existing wires *)
            let start_port, end_port = wire in
            let start_location = stack#location_of_child start_port (0, 0) in
            let end_location = stack#location_of_child end_port (0, 0) in
            match start_location, end_location with
            | Some start_location, Some end_location ->
               let x_distance = Int.abs (fst end_location - fst start_location) in
               let control_point_length = min (x_distance / 2) 100 in
               let (curve : Curve.control_point list) =
                 [
                   { point = start_location;
                     before = 0, 0;
                     after = Point.add (control_point_length, 0) start_location };
                   { point = end_location;
                     before = Point.add (-control_point_length, 0) end_location;
                     after = 0, 0; }
                 ] in
               let painter = CurvePainter.create curve in
               CurvePainter.paint view clip painter;
            | _ -> ()) wires;
        stack#paint view clip; (* draw stack *)
      end
    
    method handle (e : Event.t) ~(dirty : bool) =
      match e, dragging with
      | Event.Mouse_Move (_, (x, y)), Dragging_Frame (frame_no, offset) ->
         let ox, oy = offset in
         Array.set frame_positions frame_no (x - ox, y - oy);
         stack <- self#rebuild_stack (); true
      | Event.Mouse_Move (_, p), Dragging_Wire drag ->
         dragging <- Dragging_Wire { drag with current_location = p }; true
      | Event.Mouse_Up _, Dragging_Frame _ ->
         dragging <- NoDrag; dirty
      | Event.Mouse_Up _, Dragging_Wire _ ->
         ignore (stack#handle e ~dirty);
         dragging <- NoDrag; true
      | _ -> stack#handle e ~dirty

    method location_of_child (child_id : string) (p : Point.t) =
      if child_id = id then Some p
      else stack#location_of_child child_id p
    
  end
;;

