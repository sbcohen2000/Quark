exception Compilation_Error of string
exception Invalid_GL_Version
type t

val create : string -> string -> int * int -> t
val use : t -> unit
val destroy : t -> unit

type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
val set_matrix_4fv : t -> string -> (float, Bigarray.float32_elt) bigarray -> unit
val set_float : t -> string -> float -> unit
