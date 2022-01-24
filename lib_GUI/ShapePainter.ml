open Tgl4

type t = { vao : int;
           attrs : int list;
           n_verts : int;
           shader : Shader.t
         }
;;

let curve_to_vertex_buffer (curve : Curve.path) =
  let to_f = Float.of_int in
  let f (x, y : Point.t) =
    [| to_f x; to_f y |] in
  let all_points = Array.concat (List.map f curve) in
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    all_points
;;

let destroy (painter : t) =
  print_endline "Destroying ShapePainter";
  Shader.destroy painter.shader;
  Buffer.set_int (Gl.delete_vertex_arrays 1) painter.vao;
  List.iter (fun buffer ->
      Buffer.delete_gl_buffer buffer) painter.attrs
;;

let vert =
  "
   in vec2 vertex;

   uniform mat4 view;

   void main()
   {
   gl_Position = view * vec4(vertex, 0.0, 1.0);
   }
   "
;;

let frag =
  "
   layout(origin_upper_left) in vec4 gl_FragCoord;

   out vec4 color;

   uniform vec4 clip;

   void main()
   {
   if(gl_FragCoord.x > clip.x + clip.z || gl_FragCoord.y > clip.y + clip.w
   || gl_FragCoord.x < clip.x || gl_FragCoord.y < clip.y) {
   discard;
   }

   color = vec4(0.0, 0.0, 0.0, 1.0);
   }
   "
;;

let create (curve : Curve.t) =
  let path = Curve.create curve in
  let shader = Shader.create ~vert ~frag (3, 3) in
  let path' = (0, 0)::path @ [List.nth path 0] in
  let vao = Buffer.get_int (Gl.gen_vertex_arrays 1) in
  let n_verts = List.length path' in
  let vertices = curve_to_vertex_buffer path' in
  let vbo = Buffer.to_gl_buffer vertices in
  begin
    Gl.bind_vertex_array vao;
    Gl.bind_buffer Gl.array_buffer vbo;
    Gl.enable_vertex_attrib_array 0;
    Gl.vertex_attrib_pointer 0 2 Gl.float false 0 (`Offset 0);
    (* unbind buffers *)
    Gl.bind_vertex_array 0;
    Gl.bind_buffer Gl.array_buffer 0;
  end;
  let (painter : t) = { vao; attrs = [vbo]; n_verts; shader } in
  Gc.finalise destroy painter; painter
;;

let paint (view : Mat2.t) (clip : Rect.t) (painter : t) =
  Shader.use painter.shader;
  Shader.set_matrix_4fv painter.shader "view" (Mat2.export view);
  Shader.set_vec4 painter.shader "clip" (Rect.to_float (Mat2.apply_rect view clip));
  (* configure stencil to invert value on draw *)
  Gl.enable Gl.stencil_test;
  Gl.clear Gl.stencil_buffer_bit;
  Gl.stencil_func Gl.always 0 1;
  Gl.stencil_op Gl.keep Gl.keep Gl.invert;

  (* disable writing to color buffer *)
  Gl.color_mask false false false false;

  (* draw to the stencil buffer *)
  Gl.bind_vertex_array painter.vao;
  Gl.draw_arrays Gl.triangle_fan 0 painter.n_verts;

  (* enable color and draw into the color buffer *)
  Gl.color_mask true true true true;
  Gl.stencil_func Gl.equal 1 1;
  Gl.stencil_op Gl.keep Gl.keep Gl.keep;
  Gl.draw_arrays Gl.triangle_fan 0 painter.n_verts;
  Gl.bind_vertex_array 0;
  Gl.disable Gl.stencil_test;
;;
