type t = {
    (* The measure function returns the final
     * size of the widget. It takes two optional
     * arguments: the requested width and the requested
     * height of the widget.
     * 
     * Important: The paint function should never be
     *            called without having called measure first.
     *            Measure may store internal state in the widget
     *            which is required by paint.
     *)
    measure :
      ?requested_width:int ->
      ?requested_height:int ->
      unit ->
      (int * int);
    (* The paint function paints the widget to
     * the current OpenGL context. The function
     * takes two arguments: the current view matrix,
     * and the clipping rect to apply to the painting
     * operation. The view matrix is used to position
     * the widget at the proper place on the screen.
     * The clipping rect is relative to the widget's
     * view matrix (i.e. 0, 0 is the upper left corner
     * of the widget, not the window. *)
    paint : Mat2.t -> Rect.t -> unit;
    (* The handler function accepts an event and
     * returns whether or not the event caused the
     * widget (sub)tree to require a rebuild.
     * dirty is a parameter that is used by each
     * child widget to know if the parent widget requires
     * a rebuild.
     *
     * Important: The handler function should never
     *            directly return false! If a rebuild 
     *            is not required, the function should return dirty
     *            instead, since we don't want to deprive widgets
     *            above us their requests to rebuild.
     *)
    handler : Event.t -> dirty:bool -> bool
  }

val create_label : TextPainter.font -> string -> t ref
val create_button : TextPainter.font -> string -> t ref
val create_row : t ref list -> t ref
