exception SyntaxError of string
val parse : Char.t Stream.t -> AST.def list
