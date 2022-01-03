type t = {
    renderer : ?requested_width:float ->
               ?requested_height:float ->
               unit ->
               (float * float);
    children : t list;
    handler : Event.t -> bool
  }
;;


