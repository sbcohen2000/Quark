open Tgl4

type t = { vao : int;
           attrs : int list;
           n_verts : int;
         }
;;

let interleave (a : Curve.path) (b : Curve.path) =
  let f (ax, ay : Vec2.t) (bx, by : Vec2.t) =
    [| ax; ay; bx; by |] in
  Array.concat (List.map2 f a b)
;;

let path_to_vertex_buffer (curve : Curve.path) =
  let all_points = interleave curve curve in
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    all_points
;;

let path_to_bisector_buffer (curve : Curve.path) =
  let normals = Curve.bisectors curve in
  let negated = List.map Vec2.neg normals in
  let all_points = interleave normals negated in
  let directions = Array.init (Array.length all_points)
                     (fun idx -> Float.of_int(Int.rem idx 2)) in
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    all_points,
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    directions
;;

let vert =
  "
   layout (location = 0) in vec2 vertex;
   layout (location = 1) in vec2 normal;
   layout (location = 2) in float direction;

   out float direction_out;

   uniform mat4 view;
   uniform float stroke;

   void main()
   {
   direction_out = direction;
   gl_Position = view * vec4(vertex + (stroke * 0.5 * normal), 0.0, 1.0);
   }
   "
;;

let frag =
  "
   layout(origin_upper_left) in vec4 gl_FragCoord;
   out vec4 color;

   in float direction_out;

   uniform vec4 clip;
   uniform float stroke;

   void main()
   {
   if(gl_FragCoord.x > clip.x + clip.z || gl_FragCoord.y > clip.y + clip.w
   || gl_FragCoord.x < clip.x || gl_FragCoord.y < clip.y) {
   discard;
   }

   float edge_distance = 1 - abs(direction_out * 2 - 1);
   float h = stroke / (2 * 1.0);
   float alpha = min(h * edge_distance, 1);
   
   color = vec4(0, 0, 0, alpha);
   }
   "
;;

let shader_global = ref None

let get_shader () =
  match !shader_global with
  | None ->
     let shader = Shader.create ~vert ~frag (3, 3) in
     shader_global := Some shader;
     shader
  | Some shader -> shader
;;

let destroy (painter : t) =
  Buffer.set_int (Gl.delete_vertex_arrays 1) painter.vao;
  List.iter (fun buffer -> 
      Buffer.delete_gl_buffer buffer) painter.attrs
;;

let create (curve : Curve.t) =
  let path = Curve.create curve in
  let vao = Buffer.get_int (Gl.gen_vertex_arrays 1) in
  let n_verts = List.length path * 2 in
  let vertices = path_to_vertex_buffer path in
  let normals, directions = path_to_bisector_buffer path in
  let vbo = Buffer.to_gl_buffer vertices   in (*   vertices *)
  let nbo = Buffer.to_gl_buffer normals    in (*    normals *)
  let dbo = Buffer.to_gl_buffer directions in (* directions *)
  begin (* configure vertex attribute array *)
    Gl.bind_vertex_array vao;
    Gl.bind_buffer Gl.array_buffer vbo;
    Gl.enable_vertex_attrib_array 0;
    Gl.vertex_attrib_pointer 0 2 Gl.float false 0 (`Offset 0);
    Gl.bind_buffer Gl.array_buffer nbo;
    Gl.enable_vertex_attrib_array 1;
    Gl.vertex_attrib_pointer 1 2 Gl.float false 0 (`Offset 0);
    Gl.bind_buffer Gl.array_buffer dbo;
    Gl.enable_vertex_attrib_array 2;
    Gl.vertex_attrib_pointer 2 1 Gl.float false 0 (`Offset 0);
    (* unbind buffers *)
    Gl.bind_vertex_array 0;
    Gl.bind_buffer Gl.array_buffer 0;
  end;
  let (painter : t) = { vao; attrs = [vbo; nbo; dbo]; n_verts } in
  Gc.finalise destroy painter; painter
;;

let paint (view : Mat2.t) (clip : Rect.t) (painter : t) =
  let shader = get_shader () in
  Shader.use shader;
  Shader.set_matrix_4fv shader "view" (Mat2.export view);
  Shader.set_vec4 shader "clip" (Rect.to_float clip);
  Shader.set_float shader "stroke" 10.;
  Gl.bind_vertex_array painter.vao;
  Gl.draw_arrays Gl.triangle_strip 0 painter.n_verts;
  Gl.bind_vertex_array 0
;;
