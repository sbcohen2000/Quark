open GUI

type message = Frame_Moved of int * GFX.Point.t
             | New_Wire of string * string
             | Delete_Wire of string * string
             | Move_Wire of string * string
             | Button_Pressed of string
             | Textbox_Updated of Widgets.TextboxModel.t
;;

type model = {
    components : Widgets.component_spec list;
    wires : (string * string) list;
    textbox : Widgets.TextboxModel.t;
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

let view (ctx : message Widgets.context) (m : model) =
  ((new Widgets.textbox ctx m.textbox
      ~on_update_model:(fun tb_m -> Textbox_Updated tb_m)
   ) :> Widgets.widget)
;;

(* let view (ctx : message Widgets.context) (_m : model) = *)
(*   ((new Widgets.row ctx [ *)
(*         ((new Widgets.button ctx "hello") :> Widgets.widget); *)
(*         ((new Widgets.button ctx "hello") :> Widgets.widget); *)
(*         ((new Widgets.button ctx "hello") :> Widgets.widget); *)
(*         ((new Widgets.button ctx "hello") :> Widgets.widget); *)
(*       ]) :> Widgets.widget) *)

(* ;; *)

let handle (m : model) = function
  | Frame_Moved (frame_no, new_location) ->
     { m with components =
                    List.mapi (fun idx (component : Widgets.component_spec) ->
                        if idx = frame_no then
                          { component with location = new_location }
                        else
                          component
                      ) m.components }
  | New_Wire (start_port, end_port) ->
     { m with wires = (start_port, end_port)::m.wires }
  | Delete_Wire (start_port, end_port) ->
     { m with wires = List.filter (fun (start_port', end_port') ->
                          not (start_port = start_port'
                               && end_port = end_port')) m.wires }
  | Move_Wire (old_end_port, new_end_port) ->
     { m with wires = List.map (fun (start_port, end_port) ->
                          if end_port = old_end_port then
                            start_port, new_end_port
                          else start_port, end_port) m.wires }
  | Textbox_Updated new_model ->
     { m with textbox = new_model }
  | _ -> m
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
                 wires = [];
                 textbox = Widgets.TextboxModel.create ()
               }
;;

let () = Window.main window
;;
