open GFX
exception TODO of string

type 'a context = {
    messages : 'a Queue.t;
    mutable content_scale : float
  }
;;

open List

exception Paint_Before_Measure
class virtual widget (ctx : 'a context) (id : string option) =
        object(self)
          val id = match id with Some s -> s | None -> ""
          method get_id = id
          method private scale (v : int) =
            Float.to_int (Float.round (Float.of_int v *. ctx.content_scale))
          
          method virtual measure_impl : requested_width:(int option) ->
                                        requested_height:(int option) ->
                                        Rect.t list
          method virtual paint_impl : measurement:(Rect.t list) -> Mat2.t -> Rect.t -> unit
          method virtual handle : Event.t -> dirty:bool -> bool
          method virtual location_of_child : string -> Point.t -> Point.t option

          (* measurement is a list of rectangles, one corresponding
           * to each child object of the widget. This list should be
           * populated by the measure_impl function.
           *
           * The paint_impl function is called with the last value
           * of measurement generated by measure_impl. This allows
           * measure_impl to "layout" each of the children, and paint_impl
           * to draw each child in its layout box. *)
          val mutable measurement = ([] : Rect.t list)
          method measure ?(requested_width : int option) ?(requested_height : int option) () =
            let rects = self#measure_impl ~requested_width ~requested_height in
            measurement <- rects;
            let size = List.fold_left Rect.union (0, 0, 0, 0) rects in
            Rect.width size, Rect.height size
          
          method paint (view : Mat2.t) (clip : Rect.t) =
            if measurement = [] then
              raise Paint_Before_Measure;
            self#paint_impl ~measurement view clip;
        end
;;

class virtual container (ctx : 'a context) (id : string option) =
        object(self)
          inherit widget ctx id

          method virtual private get_child_widgets : unit -> widget list
          
          method handle (e : Event.t) ~(dirty : bool) =
            let child_widgets = self#get_child_widgets () in
            let child_rects = measurement in
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
            | Event.Key_Press _ ->
               let child_responses =
                 List.map (fun child ->
                     child#handle e ~dirty) child_widgets in
               List.exists ((=) true) child_responses || dirty
          
          method location_of_child (child_id : string) (p : Point.t) =
            if child_id = id then Some p
            else let child_widgets = self#get_child_widgets () in
                 let child_rects = measurement in
                 List.fold_left2 (fun location child rect ->
                     let child_position = Rect.x rect, Rect.y rect in
                     match location, child#location_of_child child_id
                                       (Point.add p child_position) with
                     | None, Some pos -> Some pos
                     | Some pos, _ -> Some pos
                     | _ -> None) None child_widgets child_rects
        end
;;

class spacer (ctx : 'a context) (width : int) (height : int) =
  object(self)
    inherit widget ctx None

    method measure_impl ~(requested_width  : int option)
             ~(requested_height : int option) =
      let m_width, m_height = match requested_width, requested_height with
        | Some r_width, Some r_height -> r_width, r_height
        | Some r_width, None -> r_width, self#scale height
        | None, Some r_height -> self#scale width, r_height
        | None, None -> self#scale width, self#scale height in
      [ 0, 0, m_width, m_height ]

    method paint_impl ~measurement:_ _ _ = ()
    method handle _ ~dirty = dirty
    method location_of_child (child_id : string) (p : Point.t) =
      if child_id = id then Some p else None
  end
;;

type alignment = Left | Center | Right
;;

class label (ctx : 'a context) ?(align : alignment option) (text : string) =
  let text_width, text_height = TextPainter.measure text in
  let rec shrink (requested_width : int) (text : string) =
    let width, _ = TextPainter.measure text in
    if width <= requested_width then text
    else let text' = String.sub text 0 (String.length text - 1) in
         shrink requested_width text' in
  object
    inherit widget ctx None

    method measure_impl ~(requested_width  : int option)
             ~(requested_height : int option) =
      let m_width, m_height = match requested_width, requested_height with
        | Some width, Some height ->
           width, height
        | Some width, None ->
           width, text_height
        | None, Some height ->
           text_width, height
        | None, None ->
           text_width, text_height in
      [ 0, 0, m_width, m_height ]

    method paint_impl ~(measurement : Rect.t list) (view : Mat2.t) (clip : Rect.t) =
      let _, _, width, _ = List.hd measurement in
      let text' = if width < text_width then shrink width text
                  else text in
      let text_width, text_height = TextPainter.measure text' in
      let left_padding = match align with
        | None | Some Left -> 0
        | Some Center -> 
           Int.of_float (Float.floor (Float.of_int (width - text_width) /. 2.))
        | Some Right -> width - text_width in
      let painter = TextPainter.create (left_padding, text_height / 2) text' in
      TextPainter.paint view clip painter

    method handle _ ~dirty = dirty

    method location_of_child (child_id : string) (p : Point.t) =
      if child_id = id then Some p
      else None
  end
;;

module TextboxModel : sig
  type t = string * string

  val handle_key : t -> Event.key -> t
  val handle_click : t -> int -> t
  val create : unit -> t
end =
  struct
    type t = string * string (* before cursor, after cursor *)

    let handle_key (text : string * string) (m : Event.key) =
      let before_cursor, after_cursor = text in
      let handle_backspace = function
        | "", str -> "", str
        | before, after -> String.sub before 0 (String.length before - 1), after in
      let handle_move_backward = function
        | "", str -> "", str
        | before, after ->
           let char_before_cursor = String.get before (String.length before - 1) in
           String.sub before 0 (String.length before - 1),
           Char.escaped char_before_cursor ^ after in
      let handle_move_forward = function
        | str, "" -> str, ""
        | before, after ->
           let char_after_cursor = String.get after 0 in
           before ^ Char.escaped char_after_cursor,
           String.sub after 1 (String.length after - 1) in
      let handle_kill_word = function
        | "", str -> "", str
        | before, after ->
           let next_space = String.rindex_opt before ' ' in
           match next_space with
           | Some idx -> String.sub before 0 idx, after
           | None -> "", after in
      match m with
      | Character c   -> before_cursor ^ Char.escaped c, after_cursor
      | Move_Backward -> handle_move_backward text
      | Move_Forward  -> handle_move_forward text
      | Move_Up       -> text
      | Move_Down     -> text
      | Move_Start    -> "", before_cursor ^ after_cursor
      | Move_End      -> before_cursor ^ after_cursor, ""
      | Kill_Line     -> before_cursor, ""
      | Kill_Word     -> handle_kill_word text
      | Backspace     -> handle_backspace text

    let handle_click (text : string * string) (column : int) =
      let all_text = fst text ^ snd text in
      String.sub all_text 0 column,
      String.sub all_text column (String.length all_text - column)
    
    let create () = "", ""
  end
;;

class textbox (ctx : 'a context) (model : TextboxModel.t)
        ~(on_update_model : TextboxModel.t -> 'a) =
  let (style : RectPainter.style) = {
      top_right_radius    = 0;
      bottom_right_radius = 0;
      bottom_left_radius  = 0;
      top_left_radius     = 0;
      color = 0.22, 0.46, 0.87;
      border_color = None
    } in
  let build_label (model : TextboxModel.t) =
    let before_cursor, after_cursor = model in
    new label ctx ~align:Left (before_cursor ^ after_cursor) in
  object(self)
    inherit container ctx None as super

    val label = build_label model
    
    method get_child_widgets () = [(label :> widget)]

    method measure_impl ~(requested_width : int option)
             ~(requested_height : int option) =
      let m_width, m_height = match requested_width, requested_height with
        | Some width, Some height -> label#measure ~requested_width:width ~requested_height:height ()
        | Some width, None -> label#measure ~requested_width:width ()
        | None, Some height -> label#measure ~requested_height:height ()
        | None, None -> label#measure () in
      [ 0, 0, m_width, m_height ]

    method paint_impl ~measurement:_ (view : Mat2.t) (clip : Rect.t) =
      let before_text_width, height = TextPainter.measure (fst model) in
      let cursor_rect = before_text_width - self#scale 2, 0, self#scale 4, height in
      let cursor_painter = RectPainter.create [| cursor_rect |] in
      label#paint view clip;
      RectPainter.paint view cursor_rect style cursor_painter

    method private get_click_column (state : string * string) (location : Point.t) =
      let before, after = state in
      let all_text = before ^ after in
      let loc_x, _ = location in
      let rec f (idx : int) (last_width : int) =
        if idx <= String.length all_text then
          let substring = String.sub all_text 0 idx in
          let width, _ = TextPainter.measure substring in
          if loc_x < width then
            if loc_x < last_width + (width - last_width) / 2
            then idx - 1 (* closer to prev char *)
            else idx     (* closer to next char *)
          else f (idx + 1) width
        else String.length all_text in
      f 0 0
      
    method! handle (e : Event.t) ~(dirty : bool) =
      match e with
      | Event.Key_Press key ->
         let model' = TextboxModel.handle_key model key in
         let message = on_update_model model' in
         Queue.add message ctx.messages; true
      | Event.Mouse_Down p ->
         let click_column = self#get_click_column model p in
         let model' = TextboxModel.handle_click model click_column in
         let message = on_update_model model' in
         Queue.add message ctx.messages; true
      | _ -> super#handle e ~dirty
  end

type hover_state =
  | Normal
  | Hovered
  | Pressed
;;

class button (ctx : 'a context) ?(on_press : (unit -> 'a) option) (text : string) =
  let padding = 4 in
  let child = new label ctx text in
  object(self)
    inherit widget ctx None
    
    val mutable hover_state = Normal

    method private current_style =
      let (normal_style : RectPainter.style) = {
          top_right_radius    = self#scale 7;
          bottom_right_radius = self#scale 7;
          bottom_left_radius  = self#scale 7;
          top_left_radius     = self#scale 7;
          color = 0.22, 0.22, 0.22;
          border_color = Some (0.05, 0.05, 0.05)
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
      match hover_state with
      | Normal -> normal_style
      | Hovered -> hovered_style
      | Pressed -> pressed_style
    
    method measure_impl ~(requested_width  : int option) ~(requested_height : int option) =
      let padding = self#scale padding in
      match requested_width, requested_height with
      | Some width, Some height ->
         let desired_label_width = width - padding * 2 in
         let desired_label_height = height - padding * 2 in
         ignore (child#measure ~requested_width:desired_label_width
                   ~requested_height:desired_label_height ());
         [ 0, 0, width, height ]
      | Some width, None ->
         let desired_label_width = width - padding * 2 in
         let _, label_height = child#measure ~requested_width:desired_label_width () in
         [ 0, 0, width, (label_height + padding * 2) ]
      | None, Some height ->
         let desired_label_height = height - padding * 2 in
         let label_width, _ = child#measure ~requested_height:desired_label_height () in
         [ 0, 0, (label_width + padding * 2), height ]
      | None, None ->
         let label_width, label_height = child#measure () in
         [ 0, 0, (label_width + padding * 2), (label_height + padding * 2) ]

    method paint_impl ~(measurement : Rect.t list) (view : Mat2.t) (clip : Rect.t) =
      let padding = self#scale padding in
      let _, _, width, height = List.hd measurement in
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
         hover_state <- Hovered;
         (match on_press with
          | Some cb ->
             let message = cb () in
             Queue.add message ctx.messages;
          | None -> ()); true
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

class row (ctx : 'a context) (children : widget list) =
  object
    inherit container ctx None
    
    method get_child_widgets () = children
    
    method measure_impl ~(requested_width  : int option) ~(requested_height : int option) =
      let child_rects = match requested_width, requested_height with
        | Some width, Some _ ->
           let rec layout (children : widget list) (child_rects : Rect.t list list) (curr_row : Rect.t) =
             (match children with
              | [] -> child_rects
              | child::rest ->
                 let _, y, w, h = curr_row in
                 let child_width, child_height = child#measure () in
                 if width < w + child_width then (* make a new row *)
                   let new_row = 0, y + h, child_width, child_height in
                   let child_rects' = match child_rects with
                     | head::tail ->
                        [0, y + h, child_width, child_height]::head::tail
                     | [] ->
                        [[0, y + h, child_width, child_height]] in
                   layout rest child_rects' new_row
                 else (* add to the existing row *)
                   let curr_row' = 0, y, w + child_width,
                                   if child_height > h then child_height else h in
                   let child_rects' = match child_rects with
                     | head::tail ->
                        ((w, y, child_width, child_height)::head)::tail
                     | [] ->
                        [[w, y, child_width, child_height]] in
                   layout rest child_rects' curr_row') in
           layout children [] (0, 0, 0, 0)
        | None, None ->
           let _, child_rects =
             List.fold_left (fun (width, rects) (child : widget) ->
                 let child_width, child_height = child#measure () in
                 width + child_width, (width, 0, child_width, child_height)::rects)
               (0, []) children in [ child_rects ]
        | _ -> raise (TODO "measure row with unspecified width or height") in
      let rec align (child_rects : Rect.t list list) =
        match child_rects with
        | [] -> []
        | row::rest ->
           let row_height = Rect.height 
                              (List.fold_left Rect.union (0, 0, 0, 0) row) in
           let row' = List.map (fun rect ->
                          let padding = (row_height - Rect.height rect) / 2 in
                          let x, y, w, h = rect in
                          x, padding + y, w, h) row in
           row'::(align rest) in
      List.rev (List.concat (align child_rects))

    method paint_impl ~(measurement : Rect.t list) (view : Mat2.t) (clip : Rect.t) =
      List.iter2 (fun child (px, py, _, _) ->
          let child_view = Mat2.move view px py in
          child#paint child_view clip;
        ) children measurement
    
  end
;;

class column (ctx : 'a context) (children : widget list) =
  object
    inherit container ctx None
    
    method get_child_widgets () = children
    
    method measure_impl ~(requested_width  : int option) ~(requested_height : int option) =
      match requested_width, requested_height with
      | Some _, Some height ->
         let rec layout (children : widget list) (child_rects : Rect.t list) (columns : Rect.t list) =
           (match children with
            | [] -> child_rects
            | child::rest ->
               let curr_col = List.hd columns in
               let x, _, w, h = curr_col in
               let child_width, child_height = child#measure () in
               if height < h + child_height then (* make a new row *)
                 let new_col = x + w, 0, child_width, child_height in
                 let child_rects' = (x + w, 0, child_width, child_height)::child_rects in
                 layout rest child_rects' (new_col::columns)
               else (* add to the existing row *)
                 let curr_col' = x, 0, (if child_width > w then child_width else w),
                                 h + child_height in
                 let child_rects' = (x, h, child_width, child_height)::child_rects in
                 layout rest child_rects' (curr_col'::(List.tl columns))) in
         let child_rects = layout children [] [(0, 0, 0, 0)] in
         List.rev child_rects
      | None, None ->
         let _, child_rects =
           List.fold_left (fun (height, rects) (child : widget) ->
               let child_width, child_height = child#measure () in
               height + child_height, (0, height, child_width, child_height)::rects)
             (0, []) children in
         List.rev child_rects
      | _ -> raise (TODO "measure row with unspecified width or height")

    method paint_impl ~(measurement : Rect.t list) (view : Mat2.t) (clip : Rect.t) =
      List.iter2 (fun child (px, py, _, _) ->
          let child_view = Mat2.move view px py in
          child#paint child_view clip;
        ) children measurement
    
  end
;;

class stack (ctx : 'a context) (children : (widget * Point.t) list) =
  object
    inherit container ctx None
    
    method get_child_widgets () = List.map (fun (child, _) -> child) children
    
    method measure_impl ~(requested_width  : int option) ~(requested_height : int option) =
      let _max_outline = match requested_width, requested_height with
        | Some width, Some height -> 0, 0, width, height
        | Some width, None -> 0, 0, width, 0
        | None, Some height -> 0, 0, 0, height
        | None, None -> 0, 0, 0, 0 in
      let rec layout = function
        | [] -> []
        | (child, (px, py) : widget * Point.t)::rest ->
           let child_width, child_height = child#measure () in
           let child_rect = px, py, child_width, child_height in
           child_rect::(layout rest) in
      layout children;

    method paint_impl ~measurement:_ (view : Mat2.t) (clip : Rect.t) =
      List.iter (fun (child, (px, py)) ->
          let child_view = Mat2.move view px py in
          child#paint child_view clip
        ) children
    
  end
;;

class frame (ctx : 'a context) (child : widget) ~(on_mouse_down : Point.t -> unit) =
  let title_height = 10 in
  object(self)
    inherit container ctx None as super
    
    method get_child_widgets () = [ child ]
    
    method measure_impl ~(requested_width  : int option) ~(requested_height : int option) =
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
      [ 0, title_height, child_width, child_height ]

    method paint_impl ~(measurement : Rect.t list) (view : Mat2.t) (clip : Rect.t) =
      let (title_style : RectPainter.style) =
        {
          top_right_radius    = self#scale 7;
          bottom_right_radius = 0;
          bottom_left_radius  = 0;
          top_left_radius     = self#scale 7;
          color = 0.60, 0.01, 0.12;
          border_color = None;
        } in
      let (background_style : RectPainter.style) =
        {
          top_right_radius    = self#scale 7;
          bottom_right_radius = self#scale 7;
          bottom_left_radius  = self#scale 7;
          top_left_radius     = self#scale 7;
          color = 0.15, 0.0, 0.03;
          border_color = None;
        } in
      let child_rect = List.hd measurement in
      let title_height = self#scale title_height in
      let title_rect = (self#scale 5), 0,
                       Rect.width child_rect - (self#scale 10),
                       title_height in
      let background_rect =
        let x, y, w, h = Rect.union title_rect child_rect in
        x + (self#scale 5), y, w - (self#scale 10), h in
      let title_painter = RectPainter.create [| title_rect |] in
      let background_painter = RectPainter.create [| background_rect |] in
      RectPainter.paint view clip background_style background_painter;
      RectPainter.paint view clip title_style title_painter;
      let content_view = Mat2.move view 0 title_height in
      child#paint content_view clip

    method! handle (e : Event.t) ~(dirty : bool) =
      let child_rect = List.hd measurement in
      let title_height = self#scale title_height in
      let title_bar_rect = 0, 0, Rect.width child_rect, title_height in
      match e with
      | Event.Mouse_Down p when Rect.is_inside title_bar_rect p ->
         on_mouse_down p; true
      | _ -> super#handle e ~dirty
  end
;;

class receptacle (ctx : 'a context) (id : string)
        ~(on_mouse_down : string -> unit)
        ~(on_mouse_up : string -> unit) =
  let size = 10 in
  object(self)
    inherit widget ctx (Some id)

    method measure_impl ~(requested_width : int option) ~(requested_height : int option) =
      let m_width, m_height = match requested_width, requested_height with
        | Some width, Some height -> width, height
        | Some width, None -> width, (self#scale size)
        | None, Some height -> (self#scale size), height
        | None, None -> (self#scale size), (self#scale size) in
      [ 0, 0, m_width, m_height ]

    method paint_impl ~(measurement : Rect.t list) (view : Mat2.t) (clip : Rect.t) =
      let _, _, width, height = List.hd measurement in
      let size = self#scale size in
      let (style : RectPainter.style) = {
          top_right_radius = size;
          bottom_right_radius = size;
          bottom_left_radius = size;
          top_left_radius = size;
          color = 0.8, 0.8, 0.8;
          border_color = Some (0.1, 0.1, 0.1);
        } in
      let rect = 0, 0, size, size in
      let padding_x = (width - size) / 2 in
      let padding_y = (height - size) / 2 in
      let painter = RectPainter.create [| rect |] in
      let view' = Mat2.move view padding_x padding_y in
      RectPainter.paint view' clip style painter

    method handle (e : Event.t) ~(dirty : bool) =
      match e with
      | Event.Mouse_Down _ -> on_mouse_down self#get_id; dirty
      | Event.Mouse_Up _ -> on_mouse_up self#get_id; dirty
      | _ -> dirty

    method location_of_child (child_id : string) (px, py : Point.t) =
      let size = self#scale size in
      if child_id = id then Some (px + size / 2, py + size / 2)
      else None

  end
;;

let component
      (ctx : 'a context)
      ~(inputs : string list)
      ~(outputs : string list)
      ~(on_clicked_receptacle : string -> unit)
      ~(on_released_receptacle : string -> unit) =
  let make_input_row (input : string) =
    (new row ctx [
         new receptacle ctx
           input
           ~on_mouse_down:(fun id -> on_clicked_receptacle id)
           ~on_mouse_up:(fun id -> on_released_receptacle id);
         new spacer ctx 5 0;
         new label ctx input
       ] :> widget) in
  let make_output_row (output : string) =
    (new row ctx [
         new label ctx output;
         new spacer ctx 5 0;
         new receptacle ctx
           output
           ~on_mouse_down:(fun id -> on_clicked_receptacle id)
           ~on_mouse_up:(fun id -> on_released_receptacle id)
       ] :> widget) in
  let make_input_col (inputs : string list) =
    new column ctx (List.map make_input_row inputs) in
  let make_output_col (outputs : string list) =
    new column ctx (List.map make_output_row outputs) in
  (new row ctx [
       ((make_input_col inputs) :> widget);
       new spacer ctx 10 0;
       ((make_output_col outputs) :> widget)
     ] :> widget)
;;

type drag_state =
  | NoDrag
  | Dragging_Frame of {
      frame_no : int;
      offset : Point.t;
      current_location : Point.t
    }
  | Dragging_Wire of {
      start_port : string;
      current_location : Point.t
    }
  | Moving_Wire of {
      start_port : string;
      end_port : string;
      current_location : Point.t
    }
;;

type component_spec =
  {
    location : Point.t;
    inputs   : string list;
    outputs  : string list
  }
;;

class component_graph (ctx : 'a context)
        ~(components     : component_spec list)
        ~(wires          : (string * string) list)
        ~(on_move        : int * Point.t -> 'a)
        ~(on_new_wire    : string * string -> 'a)
        ~(on_delete_wire : string * string -> 'a)
        ~(on_move_wire   : string * string -> 'a) =
  object(self)
    inherit widget ctx None

    val mutable dragging = NoDrag
    val mutable stack = new stack ctx []

    method private on_frame_mouse_down (frame_no : int) (offset : Point.t) =
      dragging <- Dragging_Frame {
                      frame_no; offset;
                      current_location = 0, 0
                    }

    method private wire_with_destination (port : string) =
      List.find_opt (fun (_, destination) ->
          destination = port) wires

    val mutable move_destination = None;
    method private build_component (spec : component_spec) =
      component ctx ~inputs:(spec.inputs) ~outputs:(spec.outputs)
        ~on_clicked_receptacle:(fun port ->
          match self#wire_with_destination port with
          | Some (start_port, end_port) -> (* wire already exists *)
             let drag = Moving_Wire {
                            start_port;
                            end_port;
                            current_location = 0, 0
                          } in
             dragging <- drag
          | None -> (* make a new wire *)
             let drag = Dragging_Wire {
                            start_port = port;
                            current_location = 0, 0
                          } in
             dragging <- drag)
        ~on_released_receptacle:(fun end_port ->
          match dragging with
          | Dragging_Wire drag ->
             let message = on_new_wire (drag.start_port, end_port) in
             Queue.add message ctx.messages;
             dragging <- NoDrag;
          | Moving_Wire _ ->
             move_destination <- Some end_port;
             dragging <- NoDrag;
          | _ -> ()),
      spec.location
    
    method private rebuild_stack () =
      let components = List.map self#build_component components in
      new stack ctx (List.mapi (fun (idx : int) (content, location : widget * Point.t) ->
                         ((new frame ctx (content :> widget)
                             ~on_mouse_down:(self#on_frame_mouse_down idx)) :> widget),
                         match dragging with
                         | Dragging_Frame f when f.frame_no = idx ->
                            Point.sub f.current_location f.offset
                         | _ -> location
                       ) components)

    initializer
      stack <- self#rebuild_stack ()

    method measure_impl ~(requested_width : int option) ~(requested_height : int option) =
      let m_width, m_height = match requested_width, requested_height with
        | Some width, Some height -> stack#measure ~requested_width:width ~requested_height:height ()
        | Some width, None -> stack#measure ~requested_width:width ()
        | None, Some height -> stack#measure ~requested_height:height ()
        | None, None -> stack#measure () in
      [ 0, 0, m_width, m_height ]
    
    method paint_impl ~measurement:_ (view : Mat2.t) (clip : Rect.t) =
      begin (* draw drag preview *)
        let preview_data = match dragging with
          | Dragging_Wire drag -> Some (drag.start_port, drag.current_location)
          | Moving_Wire drag -> Some (drag.start_port, drag.current_location)
          | _ -> None in
        match preview_data with
        | Some (start_port, current_location) ->
           let start_location = stack#location_of_child start_port (0, 0) in
           (match start_location with
            | Some start_location ->
               let distance = Float.to_int (Point.distance current_location start_location) in
               let control_point_length = min (distance / 2) 100 in
               let (curve : Curve.control_point list) =
                 [
                   { point = start_location;
                     before = 0, 0;
                     after = Point.add (control_point_length, 0) start_location };
                   { point = current_location;
                     before = Point.add (-control_point_length, 0) current_location;
                     after = 0, 0; }
                 ] in
               let painter = CurvePainter.create curve in
               CurvePainter.paint view clip painter;
            | _ -> ())
        | None -> ()
      end;
      begin
        List.iter (fun wire -> (* draw existing wires *)
            let start_port, end_port = wire in
            match dragging with
            | Moving_Wire drag when drag.start_port = start_port &&
                                      drag.end_port = end_port -> ()
            | _ ->
               let start_location = stack#location_of_child start_port (0, 0) in
               let end_location = stack#location_of_child end_port (0, 0) in
               match start_location, end_location with
               | Some start_location, Some end_location ->
                  let distance = Float.to_int (Point.distance end_location start_location) in
                  let control_point_length = min (distance / 2) 100 in
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
      | Event.Mouse_Move (_, (x, y)), Dragging_Frame drag ->
         dragging <- Dragging_Frame { drag with current_location = x, y };
         stack <- self#rebuild_stack (); true
      | Event.Mouse_Move (_, p), Dragging_Wire drag ->
         dragging <- Dragging_Wire { drag with current_location = p }; true
      | Event.Mouse_Move (_, p), Moving_Wire drag ->
         dragging <- Moving_Wire { drag with current_location = p }; true
      | Event.Mouse_Up _, Dragging_Frame drag ->
         dragging <- NoDrag;
         let message = on_move (drag.frame_no, Point.sub drag.current_location drag.offset) in
         Queue.add message ctx.messages; dirty
      | Event.Mouse_Up _, Dragging_Wire _ ->
         ignore (stack#handle e ~dirty);
         dragging <- NoDrag; true
      | Event.Mouse_Up _, Moving_Wire drag ->
         let responded = stack#handle e ~dirty in
         let message = match move_destination with
           | None -> on_delete_wire (drag.start_port, drag.end_port)
           | Some dst -> on_move_wire (drag.end_port, dst) in
         Queue.add message ctx.messages;
         move_destination <- None;
         dragging <- NoDrag; dirty || responded
      | _ -> stack#handle e ~dirty

    method location_of_child (child_id : string) (p : Point.t) =
      if child_id = id then Some p
      else stack#location_of_child child_id p
    
  end
;;

