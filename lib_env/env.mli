exception Unequal_Length
exception Not_Found

val zip : 'a list -> 'b list -> ('a * 'b) list
val zip3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val unzip : ('a * 'b) list -> ('a list * 'b list)
val unzip3 : ('a * 'b * 'c) list -> ('a list * 'b list * 'c list)
val find_opt : string -> (string * 'a) list -> 'a option
val find : string -> (string * 'a) list -> 'a
