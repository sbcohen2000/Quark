open Tgl4

type t = { vao : int;
           attrs : int list;
           n_verts : int; }
;;

let curve_to_vertex_buffer (curve : Curve.path) =
  let f (x, y : Point.t) =
    [| x; y |] in
  let all_points = Array.concat (List.map f curve) in
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    all_points
;;

let create (outline : Curve.t) =
  let path = Curve.create outline in
  let vao = Buffer.get_int (Gl.gen_vertex_arrays 1) in
  let n_verts = List.length path in
  let vertices = curve_to_vertex_buffer path in
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
  let (painter : t) = { vao; attrs = [vbo]; n_verts } in
  painter
;;

let destroy (painter : t) =
  Buffer.set_int (Gl.delete_vertex_arrays 1) painter.vao;
  List.iter (fun buffer ->
      Buffer.delete_gl_buffer buffer) painter.attrs
;;

let paint (painter : t) =
  Gl.bind_vertex_array painter.vao;
  Gl.draw_arrays Gl.line_loop 0 painter.n_verts;
  Gl.bind_vertex_array 0
;;
