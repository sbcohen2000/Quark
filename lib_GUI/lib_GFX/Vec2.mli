type t = float * float

val of_point : Point.t -> t
val to_point : t -> Point.t
val add : t -> t -> t
val sub : t -> t -> t
val neg : t -> t
val scale : t -> float -> t
val mag : t -> float
val distance : t -> t -> float
val norm : t -> t
val dot : t -> t -> float
val to_string : t -> string
