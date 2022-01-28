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

let paint (root : Widgets.widget) (window : GLFW.window) =
  (* print_endline "repaint!"; *)
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

let resize (root : Widgets.widget) window width height =
  set_content_size width height;
  paint root window;
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

let mouse_move (root : Widgets.widget) window xpos ypos =
  let p = mouse_to_coord_space xpos ypos in
  let event = Event.Mouse_Move (!last_mouse_move, p) in
  let dirty = root#handle event ~dirty:false in
  if dirty then paint root window;
  last_mouse_move := p
;;

let mouse_button (root : Widgets.widget) window _button was_press _modifiers =
  let xpos, ypos = GLFW.getCursorPos ~window in
  let p = mouse_to_coord_space xpos ypos in
  let event = if was_press then Event.Mouse_Down p
              else Event.Mouse_Up p in
  let dirty = root#handle event ~dirty:false in
  if dirty then paint root window
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

  let face = GFX.TextPainter.load_font
               ~texture:"./fonts/Geneva-13.ppm"
               ~metadata:"./fonts/Geneva-13.txt" in

  let root = new Widgets.component_graph face [
                 { inputs = ["r"; "g"; "b"; "alpha"];
                   outputs = ["color"] };
                 { inputs = ["color"];
                   outputs = ["value"] };
               ] in

  ignore (GLFW.setWindowSizeCallback ~window ~f:(Some (resize (root :> Widgets.widget))));
  ignore (GLFW.setWindowContentScaleCallback ~window ~f:(Some rescale));
  ignore (GLFW.setCursorPosCallback ~window ~f:(Some (mouse_move (root :> Widgets.widget))));
  ignore (GLFW.setMouseButtonCallback ~window ~f:(Some (mouse_button (root :> Widgets.widget))));
  
  Gl.enable Gl.blend;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;

  paint (root :> Widgets.widget) window;
  while not (GLFW.windowShouldClose ~window) do
    GLFW.waitEvents ();
  done
;;

let _ = main ()
;;
