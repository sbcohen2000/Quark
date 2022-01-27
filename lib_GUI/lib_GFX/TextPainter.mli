type font
type t

val load_font : texture:string -> metadata:string -> font
val measure : font -> string -> int * int
val create : Point.t -> font -> string -> t
val paint : Mat2.t -> Rect.t -> t -> unit
