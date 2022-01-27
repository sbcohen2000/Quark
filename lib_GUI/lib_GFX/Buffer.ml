open Tgl4

let create_bigarray k len = Bigarray.(Array1.create k c_layout len)
;;

let get_int =
  let a = create_bigarray Bigarray.int32 1 in
  fun f -> f a; Int32.to_int a.{0}
;;

let set_int =
  let a = create_bigarray Bigarray.int32 1 in
  fun f i -> a.{0} <- Int32.of_int i; f a
;;

let to_gl_buffer data =
  let id = get_int (Gl.gen_buffers 1) in
  let n_bytes = Gl.bigarray_byte_size data in
  Gl.bind_buffer Gl.array_buffer id;
  Gl.buffer_data Gl.array_buffer n_bytes (Some data) Gl.static_draw;
  id
;;

let to_string length f =
  let a = create_bigarray Bigarray.char length in
  f a; Gl.string_of_bigarray a
;;

let delete_gl_buffer id =
  set_int (Gl.delete_buffers 1) id
;;
