open GUI
open Tgl4

let file_contents_to_string (path : string) =
  let infile = open_in path in
  let rec f = fun str ->
    try 
      f (str ^ input_line infile ^ "\n")
    with End_of_file -> str in
  let s = f "" in close_in infile; s
;;

let draw (shader : Shader.t) (mesh : CurvePainter.t) (window : GLFW.window) =
  Gl.clear_color 0.8 0.8 0.8 1.;
  Gl.clear Gl.color_buffer_bit;
  Shader.use shader;
  let width, height = GLFW.getWindowSize ~window in
  let mat = Mat2.ortho ~left:0. ~right:(Float.of_int width)
              ~bottom:(Float.of_int height) ~top:0.
              ~near:(-.1.) ~far:1. in
  Shader.set_matrix_4fv shader "view" mat;
  Shader.set_float shader "stroke" 20.;
  CurvePainter.paint mesh;
  GLFW.swapBuffers ~window;
;;

let resize (shader : Shader.t) (mesh : CurvePainter.t) window _width _height =
  draw shader mesh window;
;;

let gl_version_string () =
  match Gl.get_string Gl.version with
  | None -> "error"
  | Some s -> s
;;

let (points : Curve.t) =
  [
    {
      point = 100., 200.;
      dir = 0., 1.;
      pre_extent = 0.;
      post_extent = 200.;
    };
    {
      point = 400., 200.;
      dir = 0., -1.;
      pre_extent = 200.;
      post_extent = 0.;
    };
  ]
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

  print_endline (gl_version_string ());

  let opengl_version = 3, 3 in
  let line_shader = Shader.create
                      (file_contents_to_string "line.vt")
                      (file_contents_to_string "line.fg")
                      opengl_version in
  let mesh = CurvePainter.create points in
  ignore (GLFW.setWindowSizeCallback ~window ~f:(Some (resize line_shader mesh)));

  Gl.enable Gl.blend;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;
  while not (GLFW.windowShouldClose ~window) do
    draw line_shader mesh window;
    GLFW.waitEvents ();
  done
;;

let _ = main ()
;;
