open Tgl4
open GUI

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

type message = Frame_Moved of int * GFX.Point.t
             | New_Wire of string * string
             | Delete_Wire of string * string
             | Move_Wire of string * string
             | Button_Pressed of string
;;

type model = {
    counter : int;
  }
;;

(* handler : model -> message -> model *)
let handle (model : model) = function
  | Button_Pressed "increment" ->
     { counter = model.counter + 1 }
  | Button_Pressed "decrement" ->
     { counter = model.counter - 1 }
  | _ -> model
;;

(* handle_all : context -> model -> model *)
let rec handle_all (ctx : message Widgets.context) (model : model) =
  if Queue.is_empty ctx.messages then model
  else handle_all ctx (handle model (Queue.take ctx.messages))
;;

(* view : context -> model -> widget *)
let view (ctx : message Widgets.context) (m : model) =
  new Widgets.row ctx [
      new Widgets.button ctx
        ~on_press:(fun () -> Button_Pressed "increment")
        "increment";
      new Widgets.button ctx
        ~on_press:(fun () -> Button_Pressed "decrement")
        "decrement";
      new Widgets.label ctx (Int.to_string m.counter);
    ]
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

let gl_version_string () =
  match Gl.get_string Gl.version with
  | None -> "error"
  | Some s -> s
;;

let main () =
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

  let (context : message Widgets.context) = {
      content_scale = csx;
      messages = Queue.create ()
    } in

  let (model : model) = {
      counter = 0;
    } in
  
  ignore (GLFW.setWindowSizeCallback ~window ~f:(Some resize));
  ignore (GLFW.setWindowContentScaleCallback ~window ~f:(Some rescale));
  ignore (GLFW.setCursorPosCallback ~window ~f:(Some mouse_move));
  ignore (GLFW.setMouseButtonCallback ~window ~f:(Some mouse_button));
  
  Gl.enable Gl.blend;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;

  set_root ((view context model) :> Widgets.widget);
  paint window;
  let rec loop (model : model) =
    GLFW.waitEvents ();
    if GLFW.windowShouldClose ~window then ()
    else if Queue.is_empty context.messages then
      loop model
    else
      let model' = handle_all context model in
      set_root ((view context model') :> Widgets.widget);
      paint window;
      loop model' in
  loop model
;;

let _ = main ()
;;
