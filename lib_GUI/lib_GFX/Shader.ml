open Tgl4
exception Compilation_Error of string
exception Invalid_GL_Version

type t = int (* shader id *)
;;

let glsl_version = function
  | 3,2 -> "150" | 3,3 -> "330"
  | 4,0 -> "400" | 4,1 -> "410" | 4,2 -> "420" | 4,3 -> "430" | 4,4 -> "440"
  | _ -> raise Invalid_GL_Version
;;

let compile (src : string) (typ : int) =
  let get_shader sid e = Buffer.get_int (Gl.get_shaderiv sid e) in
  let sid = Gl.create_shader typ in
  Gl.shader_source sid src;
  Gl.compile_shader sid;
  if get_shader sid Gl.compile_status = Gl.true_ then sid else
    let len = get_shader sid Gl.info_log_length in
    let log = Buffer.to_string len (Gl.get_shader_info_log sid len None) in
    (Gl.delete_shader sid; raise (Compilation_Error log))
;;

let create ~(vert : string) ~(frag : string) (gl_version : int * int) =
  let glsl_version_string = glsl_version gl_version in
  let vert_src = "#version " ^ glsl_version_string
                 ^ "core\n" ^ vert in
  let frag_src = "#version " ^ glsl_version_string
                 ^ "core\n" ^ frag in
  let vert_id = compile vert_src Gl.vertex_shader in
  let frag_id = compile frag_src Gl.fragment_shader in
  let program_id = Gl.create_program () in
  let get_program pid e = Buffer.get_int (Gl.get_programiv pid e) in
  begin
    Gl.attach_shader program_id vert_id;
    Gl.delete_shader vert_id;
    Gl.attach_shader program_id frag_id;
    Gl.delete_shader frag_id;
    Gl.link_program program_id;
  end;
  if get_program program_id Gl.link_status = Gl.true_ then program_id else
    let len = get_program program_id Gl.info_log_length in
    let log = Buffer.to_string len (Gl.get_program_info_log program_id len None) in
    (Gl.delete_program program_id; raise (Compilation_Error log))
;;

let use (shader : t) = Gl.use_program shader
;;

let destroy (shader : t) = Gl.delete_program shader
;;

type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
let set_matrix_4fv (shader : t) (name : string) (mat : (float, Bigarray.float32_elt) bigarray) =
  let location = Gl.get_uniform_location shader name in
  Gl.uniform_matrix4fv location 1 false mat;
;;

let set_vec2 (shader : t) (name : string) (v : float * float) =
  let location = Gl.get_uniform_location shader name in
  let a, b = v in
  Gl.uniform2f location a b;
;;

let set_vec3 (shader : t) (name : string) (v : float * float * float) =
  let location = Gl.get_uniform_location shader name in
  let a, b, c = v in
  Gl.uniform3f location a b c;
;;

let set_vec4 (shader : t) (name : string) (v : float * float * float * float) =
  let location = Gl.get_uniform_location shader name in
  let a, b, c, d = v in
  Gl.uniform4f location a b c d;
;;

let set_float (shader : t) (name : string) (v : float) =
  let location = Gl.get_uniform_location shader name in
  Gl.uniform1f location v;
;;

let set_int (shader : t) (name : string) (v : int) =
  let location = Gl.get_uniform_location shader name in
  Gl.uniform1i location v;
;;
