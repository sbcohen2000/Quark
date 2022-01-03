let infile = open_in "example.gen";;
let defs = Parse.parse (Stream.of_channel infile);;

List.iter (fun def ->
    match def with
    | AST.Transform (nm, formals, body) ->
       let formal_str = AST.Stringify.binding formals in
       begin
         print_endline ("transform " ^ nm ^ " (" ^ formal_str ^ ")");
         print_endline (AST.Stringify.tree body "");
         print_newline ()
       end
    | AST.Renderer (nm, formals, body) ->
       let formal_str = AST.Stringify.binding formals in
       begin
         print_endline ("renderer: " ^ nm ^ " (" ^ formal_str ^ ")");
         print_endline (AST.Stringify.tree body "");
         print_newline ()
       end
    | AST.Decl (nm, body) ->
       begin
         print_endline ("decl: " ^ nm);
         print_endline (AST.Stringify.tree body "");
         print_newline ()
       end) defs;;
