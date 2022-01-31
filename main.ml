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

let root_node = ref None

let get_root_node (ctx : Widgets.context) =
  match !root_node with
  | None -> new Widgets.label ctx "Missing Root Node"
  | Some root -> root
;;

let rec set_root_node
          (context : Widgets.context)
          ~(components : Widgets.component_spec list)
          ~(wires : (string * string) list) =
  let new_root = new Widgets.component_graph context
                   ~components ~wires
                   ~on_move:(fun (component_no, location) ->
                     set_root_node context
                       ~components:(List.mapi (fun n component ->
                                        if n = component_no then
                                          ({ component with location; } : Widgets.component_spec)
                                        else component) components)
                       ~wires)
                   ~on_new_wire:(fun (source, destination) ->
                     set_root_node context ~components
                       ~wires:((source, destination)::wires))
                   ~on_delete_wire:(fun (source, destination) ->
                     set_root_node context ~components
                       ~wires:(List.filter (fun (src, dst) ->
                                   not (source = src && destination = dst)) wires))
                   ~on_move_wire:(fun (old_dst, new_dst) ->
                     set_root_node context ~components
                       ~wires:(List.map (fun (src, dst) ->
                                   if dst = old_dst then src, new_dst
                                   else src, dst) wires)) in
  ignore (new_root#measure ());
  root_node := Some (new_root)
;;

let paint (ctx : Widgets.context) (window : GLFW.window) =
  let root = get_root_node ctx in
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

let resize (ctx : Widgets.context) window width height =
  set_content_size width height;
  paint ctx window;
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

let mouse_move (ctx : Widgets.context) window xpos ypos =
  let root = get_root_node ctx in
  let p = mouse_to_coord_space xpos ypos in
  let event = Event.Mouse_Move (!last_mouse_move, p) in
  let dirty = root#handle event ~dirty:false in
  if dirty then paint ctx window;
  last_mouse_move := p
;;

let mouse_button (ctx : Widgets.context) window _button was_press _modifiers =
  let root = get_root_node ctx in
  let xpos, ypos = GLFW.getCursorPos ~window in
  let p = mouse_to_coord_space xpos ypos in
  let event = if was_press then Event.Mouse_Down p
              else Event.Mouse_Up p in
  let dirty = root#handle event ~dirty:false in
  if dirty then paint ctx window
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

  let (context : Widgets.context) = {
      content_scale = csx;
    } in

  set_root_node context
    ~components:[
      { location = 100, 100;
        inputs = ["a"; "b"];
        outputs = ["c"] };
      { location = 200, 100;
        inputs = ["d"; "e"];
        outputs = ["f"] };
      { location = 300, 100;
        inputs = ["g"; "h"];
        outputs = ["i"] };
      { location = 400, 100;
        inputs = ["j"; "k"];
        outputs = ["l"] };
      { location = 500, 100;
        inputs = ["m"; "n"];
        outputs = ["o"] }
    ]
    ~wires:[];
  
  ignore (GLFW.setWindowSizeCallback ~window
            ~f:(Some (resize context)));
  ignore (GLFW.setWindowContentScaleCallback ~window
            ~f:(Some rescale));
  ignore (GLFW.setCursorPosCallback ~window
            ~f:(Some (mouse_move context)));
  ignore (GLFW.setMouseButtonCallback ~window
            ~f:(Some (mouse_button context)));
  
  Gl.enable Gl.blend;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;

  paint context window;
  while not (GLFW.windowShouldClose ~window) do
    GLFW.waitEvents ();
  done
;;

let _ = main ()
;;
