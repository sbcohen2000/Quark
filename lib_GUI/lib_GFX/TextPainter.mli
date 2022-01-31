type font
type t

val load_font : texture:string -> metadata:string -> font
val measure : string -> int * int
val create : Point.t -> string -> t
val paint : Mat2.t -> Rect.t -> t -> unit
