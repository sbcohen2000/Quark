type ('a, 'b) t

val create : handle:('model -> 'message -> 'model) ->
             view:('message Widgets.context -> 'model -> Widgets.widget) ->
             initial_model:'model -> ('model, 'message) t

val main : ('model, 'message) t -> unit
