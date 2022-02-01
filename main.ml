open GUI

type message = Frame_Moved of int * GFX.Point.t
             | New_Wire of string * string
             | Delete_Wire of string * string
             | Move_Wire of string * string
             | Button_Pressed of string
;;

type model = {
    counter : int;
  }
;;

let view (ctx : message Widgets.context) (m : model) =
  (new Widgets.row ctx [
      new Widgets.button ctx
        ~on_press:(fun () -> Button_Pressed "increment")
        "increment";
      new Widgets.button ctx
        ~on_press:(fun () -> Button_Pressed "decrement")
        "decrement";
      new Widgets.label ctx (Int.to_string m.counter);
     ] :> Widgets.widget)
;;

let handle (model : model) = function
  | Button_Pressed "increment" ->
     { counter = model.counter + 1 }
  | Button_Pressed "decrement" ->
     { counter = model.counter - 1 }
  | _ -> model
;;

let window = Window.create ~handle ~view
               ~initial_model:{ counter = 0 }
;;

let () = Window.main window
;;
