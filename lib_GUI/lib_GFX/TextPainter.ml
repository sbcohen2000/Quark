open Tgl4

type glyph_record = {
    bearing : int;
    bounds_start : int;
    bounds_end : int;
    advance : int;
  }
;;

type font = {
    texture_buffer : int;
    texture_buffer_height : int;
    texture_buffer_width : int;
    metadata : (char, glyph_record) Hashtbl.t;
    shader : Shader.t;
  }
;;

type t = {
    vao : int;
    attrs : int list;
    n_rects : int
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

let load_ppm (path : string) =
  let infile = open_in path in
  ignore (input_line infile);
  let size_str = String.split_on_char ' ' (input_line infile) in
  let width = int_of_string (List.nth size_str 0) in
  let height = int_of_string (List.nth size_str 1) in
  ignore (input_line infile);
  let pixels = Array.init (width * height) (fun _idx ->
                   let line = input_line infile in
                   let stop = String.index line ' ' in
                   let first_number = String.sub line 0 stop in
                   int_of_string first_number) in
  close_in infile;
  (width, height,
   Bigarray.Array1.of_array Bigarray.Int8_unsigned Bigarray.c_layout pixels)
;;

exception BadFile of string
let load_metadata (path : string) =
  let infile = open_in path in
  let table = Hashtbl.create 128 in
  let n_glyphs = int_of_string (input_line infile) in
  ignore (input_line infile); (* ignore size *)
  let rec read_record (n : int) =
    if n = 0 then ()
    else let c = input_char infile in
         let rest = input_line infile in
         match String.split_on_char ' ' rest with
         | [_; bearing; bounds_start; bounds_end; advance] ->
            let bearing = int_of_string bearing in
            let bounds_start = int_of_string bounds_start in
            let bounds_end = int_of_string bounds_end in
            let advance = int_of_string advance in
            let (record : glyph_record) = {
                bearing; bounds_start; bounds_end; advance
              } in
            Hashtbl.add table c record;
            read_record (n - 1)
         | _ -> raise (BadFile ("can't parse glyph " ^ Char.escaped c)) in
  read_record n_glyphs; table
;;

let to_gl_texture ~width ~height data =
  let id = Buffer.get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d id;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_border;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_border;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.pixel_storei Gl.unpack_alignment 1;
  Gl.tex_image2d Gl.texture_2d 0 Gl.red width height 0 Gl.red
    Gl.unsigned_byte (`Data data);
  Gl.generate_mipmap Gl.texture_2d; id
;;

let vert =
  "
   layout (location = 0) in vec2 vertex;
   layout (location = 1) in vec2 position;
   layout (location = 2) in vec2 size;
   layout (location = 3) in float offset;
   
   out vec2 vertex_out;
   out float rect_width;
   out float rect_offset;

   uniform mat4 view;
   
   void main()
   {
   vertex_out = vertex;
   rect_width = size.x;
   rect_offset = offset;

   vec2 scaled = vec2(vertex.x * size.x, vertex.y * size.y);
   gl_Position = view * vec4(scaled + position, 0.0, 1.0);
   }
   "
;;

let frag =
  "
   layout(origin_upper_left) in vec4 gl_FragCoord;
   in vec2 vertex_out;
   in float rect_width;
   in float rect_offset;

   out vec4 color;

   uniform vec4 clip;
   uniform sampler2D font_texture;
   uniform float font_texture_length;
   uniform float font_texture_height;

   float blurred_texture(vec2 loc) {
   int size = 1;
   float value = 0;
   for(int i = -size; i <= size; ++i) {
   for(int j = -size; j <= size; ++j) {
   float dx = loc.x + (1.0 / font_texture_length) * i;
   float dy = loc.y + (1.0 / font_texture_height) * j;
   value += texture(font_texture, vec2(dx, dy)).r;
   }
   }
   return value / float((size * 2 + 1) * (size * 2 + 1));
   }

   void main()
   {
   if(gl_FragCoord.x > clip.x + clip.z || gl_FragCoord.y > clip.y + clip.w
   || gl_FragCoord.x < clip.x || gl_FragCoord.y < clip.y) {
   discard;
   }

   float x = vertex_out.x * (rect_width / font_texture_length)
   + (rect_offset / font_texture_length);
   vec2 slice = vec2(x, vertex_out.y);

   float raw_value = texture(font_texture, slice).r;
   /* TODO: shadow not hooked up right now */
   float shadow = blurred_texture(vec2(slice.x, slice.y));
   color = vec4(1, 1, 1, raw_value);
   }
   "
;;

let destroy_font (face : font) =
  Shader.destroy face.shader;
  Buffer.set_int (Gl.delete_textures 1) face.texture_buffer
;;

let load_font ~(texture : string) ~(metadata : string) =
  let width, height, texture_data = load_ppm texture in
  let metadata = load_metadata metadata in
  let texture_buffer = to_gl_texture ~width ~height texture_data in
  let shader = Shader.create ~vert ~frag (3, 3) in
  let (face : font) = { texture_buffer_width = width;
                        texture_buffer_height = height;
                        texture_buffer; metadata; shader } in
  Gc.finalise destroy_font face; face
;;

let face_global = ref None
;;

let get_face () =
  match !face_global with
  | None ->
     let face = load_font
                  ~texture:"./fonts/Geneva-13.ppm"
                  ~metadata:"./fonts/Geneva-13.txt" in
     face_global := Some face;
     face
  | Some face -> face
;;

(* returns the new pen_x location along with the glyph's rect *)
let rect_of_char (face : font) (c : char) (pen_x : int) (pen_y : int) =
  let metrics = Hashtbl.find face.metadata c in
  let height = face.texture_buffer_height in
  let rect = pen_x, (pen_y - height / 2), metrics.advance, height in
  let pen_x' = pen_x + metrics.advance in
  pen_x', rect
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

let string_to_offsets_buffer (face : font) (text : string) =
  let a = Array.init (String.length text) (fun idx ->
              let c = String.get text idx in
              let metrics = Hashtbl.find face.metadata c in
              Float.of_int metrics.bounds_start) in
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout a
;;

let destroy_painter (painter : t) =
  Buffer.set_int (Gl.delete_vertex_arrays 1) painter.vao;
  List.iter (fun buffer ->
      Buffer.delete_gl_buffer buffer) painter.attrs
;;

let measure (text : string) =
  let face = get_face () in
  Seq.fold_left (fun pen_x c ->
      let pen_x', _ = rect_of_char face c pen_x 0 in
      pen_x') 0 (String.to_seq text),
  face.texture_buffer_height
;;

let create (px, py : Point.t) (text : string) =
  let face = get_face () in
  let pen_y = py in
  let pen_x = ref px in
  let rects = Array.init (String.length text) (fun idx ->
                  let c = String.get text idx in
                  let pen_x', rect = rect_of_char face c !pen_x pen_y in
                  pen_x := pen_x'; rect) in
  let n_rects = Array.length rects in
  let vao = Buffer.get_int (Gl.gen_vertex_arrays 1) in
  let vbo = Buffer.to_gl_buffer rect_verts in
  let pos_buf = Buffer.to_gl_buffer (rects_to_pos_buffer rects) in 
  let size_buf = Buffer.to_gl_buffer (rects_to_size_buffer rects) in
  let offset_buf = Buffer.to_gl_buffer (string_to_offsets_buffer face text) in
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
    Gl.bind_buffer Gl.array_buffer offset_buf;
    Gl.enable_vertex_attrib_array 3;
    Gl.vertex_attrib_pointer 3 1 Gl.float false 0 (`Offset 0);
    Gl.vertex_attrib_divisor 1 1;
    Gl.vertex_attrib_divisor 2 1;
    Gl.vertex_attrib_divisor 3 1;
    (* unbind buffers *)
    Gl.bind_vertex_array 0;
    Gl.bind_buffer Gl.array_buffer 0;
  end;
  let (painter : t) = { vao; attrs = [vbo; pos_buf;
                                      size_buf; offset_buf];
                        n_rects } in
  Gc.finalise destroy_painter painter; painter
;;

let paint (view : Mat2.t) (clip : Rect.t) (painter : t) =
  let face = get_face () in
  Shader.use face.shader;
  Shader.set_matrix_4fv face.shader "view" (Mat2.export view);
  Shader.set_vec4 face.shader "clip" (Rect.to_float clip);
  Shader.set_int face.shader "font_texture" 0;
  Shader.set_float face.shader "font_texture_length"
    (Float.of_int face.texture_buffer_width);
  Shader.set_float face.shader "font_texture_height"
    (Float.of_int face.texture_buffer_height);
  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d face.texture_buffer;
  Gl.bind_vertex_array painter.vao;
  Gl.draw_arrays_instanced Gl.triangle_fan 0 4 painter.n_rects;
  Gl.bind_vertex_array 0
;;
