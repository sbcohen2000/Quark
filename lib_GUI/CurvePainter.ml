open Tgl4

type t = { vao : int;
           attrs : int list;
           n_verts : int; }
;;

let interleave (a : Curve.path) (b : Curve.path) =
  let f (ax, ay : Point.t) (bx, by : Point.t) =
    [| ax; ay; bx; by |] in
  Array.concat (List.map2 f a b)
;;

let curve_to_vertex_buffer (curve : Curve.path) =
  let all_points = interleave curve curve in
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    all_points
;;

let curve_to_bisector_buffer (curve : Curve.path) =
  let normals = Curve.bisectors curve in
  let negated = List.map Point.neg normals in
  let all_points = interleave normals negated in
  let directions = Array.init (Array.length all_points)
                     (fun idx -> Float.of_int(Int.rem idx 2)) in
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    all_points,
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    directions
;;

let create (curve : Curve.t) =
  let path = Curve.create curve in
  let vao = Buffer.get_int (Gl.gen_vertex_arrays 1) in
  let n_verts = List.length path * 2 in
  let vertices = curve_to_vertex_buffer path in
  let normals, directions = curve_to_bisector_buffer path in
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
  painter
;;

let destroy (painter : t) =
  Buffer.set_int (Gl.delete_vertex_arrays 1) painter.vao;
  List.iter (fun buffer -> 
      Buffer.delete_gl_buffer buffer) painter.attrs
;;

let paint (painter : t) =
  Gl.bind_vertex_array painter.vao;
  Gl.draw_arrays Gl.triangle_strip 0 painter.n_verts;
  Gl.bind_vertex_array 0
;;
