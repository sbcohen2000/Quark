type t = int * int

val add : t -> t -> t
val sub : t -> t -> t
val neg : t -> t
val scale : t -> float -> t
val mag : t -> float
val distance : t -> t -> float
val norm : t -> t
val dot : t -> t -> int
val to_float : t -> float * float
