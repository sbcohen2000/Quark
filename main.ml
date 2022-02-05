open GUI

type message = Frame_Moved of int * GFX.Point.t
             | New_Wire of string * string
             | Delete_Wire of string * string
             | Move_Wire of string * string
             | Button_Pressed of string
;;

type model = {
    components : Widgets.component_spec list;
    wires : (string * string) list;
  }
;;

(* let view (ctx : message Widgets.context) (m : model) = *)
(*   (new Widgets.component_graph ctx *)
(*      ~components:m.components *)
(*      ~wires:m.wires *)
(*      ~on_move:(fun (frame_no, new_location) -> *)
(*        Frame_Moved (frame_no, new_location)) *)
(*      ~on_new_wire:(fun (start_port, end_port) -> *)
(*        New_Wire (start_port, end_port)) *)
(*      ~on_delete_wire:(fun (start_port, end_port) -> *)
(*        Delete_Wire (start_port, end_port)) *)
(*      ~on_move_wire:(fun (old_end_port, new_end_port) -> *)
(*        Move_Wire (old_end_port, new_end_port)) *)
(*    :> Widgets.widget) *)
(* ;; *)

let view (ctx : message Widgets.context) (_m : model) =
  ((new Widgets.textbox ctx ("Hello", "")) :> Widgets.widget)
;;

let handle (model : model) = function
  | Frame_Moved (frame_no, new_location) ->
     { model with components =
                    List.mapi (fun idx (component : Widgets.component_spec) ->
                        if idx = frame_no then
                          { component with location = new_location }
                        else
                          component
                      ) model.components }
  | New_Wire (start_port, end_port) ->
     { model with wires = (start_port, end_port)::model.wires }
  | Delete_Wire (start_port, end_port) ->
     { model with wires = List.filter (fun (start_port', end_port') ->
                              not (start_port = start_port'
                                   && end_port = end_port')) model.wires }
  | Move_Wire (old_end_port, new_end_port) ->
     { model with wires = List.map (fun (start_port, end_port) ->
                              if end_port = old_end_port then
                                start_port, new_end_port
                              else start_port, end_port) model.wires }
  | _ -> model
;;

let window = Window.create ~handle ~view
               ~initial_model:{
                 components = [
                   { location = 0, 0;
                     inputs = ["a"; "b"; "c"];
                     outputs = ["output1"] };
                   { location = 100, 100;
                     inputs = ["x"; "y"; "z"];
                     outputs = ["output2"] }
                 ];
                 wires = []
               }
;;

let () = Window.main window
;;
