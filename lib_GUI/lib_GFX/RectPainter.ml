open Tgl4

type style = {
    top_right_radius    : int;
    bottom_right_radius : int;
    bottom_left_radius  : int;
    top_left_radius     : int;
    color               : (float * float * float);
    border_color        : (float * float * float) option;
  }
;;

type t = {
    vao : int;
    attrs : int list;
    n_rects : int;
  }
;;

let rect_verts =
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    [|
      1.; 1.;
      1.; 0.;
      0.; 0.;
      0.; 1.;
    |]
;;

let vert =
  "
   layout (location = 0) in vec2 vertex;
   layout (location = 1) in vec2 position;
   layout (location = 2) in vec2 size;

   out vec2 size_out;
   out vec2 coord;

   uniform mat4 view;

   void main()
   {
   size_out = size;
   coord = vertex;

   vec2 scaled = vec2(vertex.x * size.x, vertex.y * size.y);
   gl_Position = view * vec4(scaled + position, 0.0, 1.0);
   }
   "
;;

let frag =
  "
   layout(origin_upper_left) in vec4 gl_FragCoord;
   in vec2 size_out;
   in vec2 coord;

   out vec4 color;

   uniform vec4 clip;
   uniform vec4 radii;
   uniform vec3 fill_color;
   uniform vec4 border_color;

   /* https://www.iquilezles.org/
   *     www/articles/distfunctions2d/distfunctions2d.htm
   */
   float sdRoundedBox(in vec2 p, in vec2 b, in vec4 r)
   {
   r.xy = (p.x>0.0)?r.xy : r.zw;
   r.x  = (p.y>0.0)?r.x  : r.y;
   vec2 q = abs(p)-b+r.x;
   return min(max(q.x,q.y),0.0) + length(max(q,0.0)) - r.x;
   }

   void main()
   {
   if(gl_FragCoord.x > clip.x + clip.z || gl_FragCoord.y > clip.y + clip.w
   || gl_FragCoord.x < clip.x || gl_FragCoord.y < clip.y) {
   discard;
   }

   float scale = size_out.y;
   vec2 p = (2.0 * coord * size_out - size_out) / scale;
   float d = sdRoundedBox(p, vec2(size_out.x / size_out.y, 1), radii / scale);
   float interior = 1.0 - smoothstep(0.0, 3.0 / scale, max(d + 3.0 / scale, 0.0));
   float border = 1.0 - smoothstep(0.0, 2.0 / scale, abs(d + 2.0 / scale));
   vec4 c = vec4(fill_color, 1.0);
   c = mix(c, border_color, border);
   color = vec4(c.rgb, c.a * interior);
   }
   "
;;

let shader_global = ref None
;;

let get_shader () =
  match !shader_global with
  | None ->
     let shader = Shader.create ~vert ~frag (3, 3) in
     shader_global := Some shader;
     shader
  | Some shader -> shader
;;

let rects_to_pos_buffer (rects : Rect.t array) =
  let a = Array.init (Array.length rects * 2) (fun idx ->
              if Int.rem idx 2 = 0
              then Float.of_int (Rect.x (Array.get rects (idx / 2)))
              else Float.of_int (Rect.y (Array.get rects ((idx - 1) / 2)))) in
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout a
;;

let rects_to_size_buffer (rects : Rect.t array) =
  let a = Array.init (Array.length rects * 2) (fun idx ->
              if Int.rem idx 2 = 0
              then Float.of_int (Rect.width (Array.get rects (idx / 2)))
              else Float.of_int (Rect.height (Array.get rects ((idx - 1) / 2)))) in
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout a
;;

let destroy (painter : t) =
  Buffer.set_int (Gl.delete_vertex_arrays 1) painter.vao;
  List.iter (fun buffer ->
      Buffer.delete_gl_buffer buffer) painter.attrs
;;

let create (rects : Rect.t array) =
  let n_rects = Array.length rects in
  let vao = Buffer.get_int (Gl.gen_vertex_arrays 1) in
  let vbo = Buffer.to_gl_buffer rect_verts in
  let pos_buf = Buffer.to_gl_buffer (rects_to_pos_buffer rects) in 
  let size_buf = Buffer.to_gl_buffer (rects_to_size_buffer rects) in
  begin
    Gl.bind_vertex_array vao;
    Gl.bind_buffer Gl.array_buffer vbo;
    Gl.enable_vertex_attrib_array 0;
    Gl.vertex_attrib_pointer 0 2 Gl.float false 0 (`Offset 0);
    Gl.bind_buffer Gl.array_buffer pos_buf;
    Gl.enable_vertex_attrib_array 1;
    Gl.vertex_attrib_pointer 1 2 Gl.float false 0 (`Offset 0);
    Gl.bind_buffer Gl.array_buffer size_buf;
    Gl.enable_vertex_attrib_array 2;
    Gl.vertex_attrib_pointer 2 2 Gl.float false 0 (`Offset 0);
    Gl.vertex_attrib_divisor 1 1;
    Gl.vertex_attrib_divisor 2 1;
    (* unbind buffers *)
    Gl.bind_vertex_array 0;
    Gl.bind_buffer Gl.array_buffer 0;
  end;
  let (painter : t) = { vao; attrs = [vbo; pos_buf; size_buf];
                        n_rects } in
  Gc.finalise destroy painter; painter
;;

let paint (view : Mat2.t) (clip : Rect.t) (style : style) (painter : t) =
  let shader = get_shader () in
  Shader.use shader;
  Shader.set_matrix_4fv shader "view" (Mat2.export view);
  Shader.set_vec4 shader "clip" (Rect.to_float clip);
  Shader.set_vec4 shader "radii" (Float.of_int style.bottom_right_radius,
                                  Float.of_int style.top_right_radius,
                                  Float.of_int style.bottom_left_radius,
                                  Float.of_int style.top_left_radius);
  Shader.set_vec3 shader "fill_color" style.color;
  Shader.set_vec4 shader "border_color"
    (match style.border_color with
     | Some (r, g, b) -> (r, g, b, 1.)
     | None -> (0., 0., 0., 0.));
  Gl.bind_vertex_array painter.vao;
  Gl.draw_arrays_instanced Gl.triangle_fan 0 4 painter.n_rects;
  Gl.bind_vertex_array 0
;;
