open Tgl4

type ('model, 'message) t = {
    handle : 'model -> 'message -> 'model;
    view : 'message Widgets.context -> 'model -> Widgets.widget;
    initial_model : 'model;
  }
;;

let create
      ~(handle : 'model -> 'message -> 'model)
      ~(view   : 'message Widgets.context -> 'model -> Widgets.widget)
      ~(initial_model : 'model) =
  ({ handle; view; initial_model } : ('model, 'message) t)
;;

let set_content_scale, get_content_scale =
  let scale = ref (1., 1.) in
  (fun (x : float) (y : float) -> scale := x, y),
  fun () -> !scale
;;

let set_content_size, get_content_size =
  let size = ref (0, 0) in
  (fun (width : int) (height : int) -> size := width, height),
  fun () -> !size
;;

let rec handle_all (interface : ('model, 'message) t) (ctx : 'message Widgets.context) (model : 'model) =
  if Queue.is_empty ctx.messages then model
  else handle_all interface ctx (interface.handle model (Queue.take ctx.messages))
;;

let get_root, set_root =
  let root = ref None in
  (fun () ->
  match !root with
  | None -> new Widgets.label {
                content_scale = 1.;
                messages = Queue.create ()
              } "no root"
  | Some root -> root),
  fun (widget : Widgets.widget) ->
  root := Some widget
;;

let paint (window : GLFW.window) =
  let root = get_root () in
  Gl.clear_color 0.1 0.1 0.1 1.;
  Gl.clear Gl.color_buffer_bit;
  let window_width, window_height = get_content_size () in
  let sx, sy = get_content_scale () in
  let width  = Int.of_float (Float.round (Float.of_int window_width  *. sx)) in
  let height = Int.of_float (Float.round (Float.of_int window_height *. sy)) in
  let view = GFX.Mat2.identity width height in
  ignore (root#measure ~requested_width:width ~requested_height:height ());
  ignore (root#paint view (0, 0, width, height));
  GLFW.swapBuffers ~window;
;;

let resize window width height =
  set_content_size width height;
  paint window;
;;

let rescale _window csx csy =
  set_content_scale csx csy;
;;

let mouse_to_coord_space (xpos : float) (ypos : float) =
  let sx, sy = get_content_scale () in
  let x = Int.of_float (Float.round (xpos *. sx)) in
  let y = Int.of_float (Float.round (ypos *. sy)) in
  x, y
;;

let last_mouse_move = ref (0, 0)
;;

let mouse_move window xpos ypos =
  let root = get_root () in
  let p = mouse_to_coord_space xpos ypos in
  let event = Event.Mouse_Move (!last_mouse_move, p) in
  let dirty = root#handle event ~dirty:false in
  if dirty then paint window;
  last_mouse_move := p
;;

let mouse_button window _button was_press _modifiers =
  let root = get_root () in
  let xpos, ypos = GLFW.getCursorPos ~window in
  let p = mouse_to_coord_space xpos ypos in
  let event = if was_press then Event.Mouse_Down p
              else Event.Mouse_Up p in
  let dirty = root#handle event ~dirty:false in
  if dirty then paint window
;;

let key_press window (key : GLFW.key) (_scancode : int)
      (action : GLFW.key_action) (mods : GLFW.key_mod list) =
  match action with
  | GLFW.Release -> ()
  | GLFW.Press | GLFW.Repeat ->
     let key_event = match mods, key with
       | [GLFW.Control], GLFW.B | _, GLFW.Left ->
          Some Event.Move_Backward
       | [GLFW.Control], GLFW.F | _, GLFW.Right ->
          Some Event.Move_Forward
       | [GLFW.Control], GLFW.P | _, GLFW.Up ->
          Some Event.Move_Up
       | [GLFW.Control], GLFW.N | _, GLFW.Down ->
          Some Event.Move_Down
       | [GLFW.Control], GLFW.A | _, GLFW.Home ->
          Some Event.Move_Start
       | [GLFW.Control], GLFW.E | _, GLFW.End ->
          Some Event.Move_End
       | [GLFW.Control], GLFW.K ->
          Some Event.Kill_Line
       | [GLFW.Control], GLFW.Backspace ->
          Some Event.Kill_Word
       | _, GLFW.Backspace ->
          Some Event.Backspace
       | [GLFW.Control], GLFW.Space ->
          Some Event.Set_Selection
       | _ -> None in
     match key_event with
     | Some key ->
        let root = get_root () in
        let dirty = root#handle (Event.Key_Press key) ~dirty:false in
        if dirty then paint window
     | None -> ()
;;

let char_key_press window (codepoint : int) =
  let root = get_root () in
  let c = Char.chr codepoint in
  let dirty = root#handle (Event.Key_Press (Event.Character c)) ~dirty:false in
  if dirty then paint window
;;  

let gl_version_string () =
  match Gl.get_string Gl.version with
  | None -> "error"
  | Some s -> s
;;

let main (interface : ('model, 'message) t) =
  GLFW.init ();
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:3;
  GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;
  GLFW.windowHint ~hint:GLFW.OpenGLForwardCompat ~value:true;
  at_exit GLFW.terminate;
  let window = GLFW.createWindow ~width:640 ~height:480 ~title:"Quark" () in
  GLFW.makeContextCurrent ~window:(Some window);

  let gl_version = gl_version_string () in
  let csx, csy = GLFW.getWindowContentScale ~window:window in
  set_content_scale csx csy;
  set_content_size 640 480;
  print_endline ("OpenGL version: " ^ gl_version);
  print_endline ("Content scale: " ^ Float.to_string csx ^ ", " ^ Float.to_string csy);

  let (context : 'message Widgets.context) = {
      content_scale = csx;
      messages = Queue.create ()
    } in

  ignore (GLFW.setWindowSizeCallback ~window ~f:(Some resize));
  ignore (GLFW.setWindowContentScaleCallback ~window ~f:(Some rescale));
  ignore (GLFW.setCursorPosCallback ~window ~f:(Some mouse_move));
  ignore (GLFW.setMouseButtonCallback ~window ~f:(Some mouse_button));
  ignore (GLFW.setKeyCallback ~window ~f:(Some key_press));
  ignore (GLFW.setCharCallback ~window ~f:(Some char_key_press));

  Gl.enable Gl.blend;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;

  set_root ((interface.view context (interface.initial_model)) :> Widgets.widget);
  paint window;
  let rec loop (model : 'model) =
    GLFW.waitEvents ();
    if GLFW.windowShouldClose ~window then ()
    else if Queue.is_empty context.messages then
      loop model
    else
      let model' = handle_all interface context model in
      set_root ((interface.view context model') :> Widgets.widget);
      paint window;
      loop model' in
  loop interface.initial_model
;;

