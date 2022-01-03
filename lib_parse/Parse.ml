module Generator_Token =
  struct
    type t =
      | Open_Paren
      | Close_Paren
      | Open_Angle
      | Close_Angle
      | Open_Square
      | Close_Square
      | Colon
      | Arrow
      | Identifier of string
      | Int        of int
      | Float      of float
      | Def
      | Transform
      | Renderer
      | Fun
      | Let

    type response =
      | Done  of t
      | If    of Char.t * response * response
      | Until of (Char.t -> bool) * (string -> t)
      | While of (Char.t -> bool) * (string -> t)
      | Nothing

    let is_numeric (c : Char.t) =
      c >= '0' && c <= '9'
    
    let is_valid_identifier_starter (c : Char.t) =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

    let is_valid_identifier_char (c : Char.t) =
      is_valid_identifier_starter c || is_numeric c
      || c = '-' || c = '_' || c = '?'

    let make_keyword_or_identifier = function
      | "def" -> Def
      | "transform" -> Transform
      | "renderer" -> Renderer
      | "fun" -> Fun
      | "let" -> Let
      | str -> Identifier str
    
    let of_chars = function
      | '(' -> Done Open_Paren
      | ')' -> Done Close_Paren
      | '<' -> Done Open_Angle
      | '>' -> Done Close_Angle
      | '[' -> Done Open_Square
      | ']' -> Done Close_Square
      | ':' -> Done Colon
      | '-' -> If ('>', Done Arrow, Nothing)
      | c -> if is_valid_identifier_starter c
             then
               While (is_valid_identifier_char,
                      make_keyword_or_identifier)
             else Nothing

    let of_int   (i :   int) = Int i
    let of_float (f : float) = Float f

    let equal_kind (a : t) (b : t) =
      let strip = function
        | Identifier _ -> Identifier ""
        | Int        _ -> Int        0
        | Float      _ -> Float      0.
        | tkn -> tkn in
      strip a = strip b
    
    let to_string = function
      | Open_Paren   -> "("
      | Close_Paren  -> ")"
      | Open_Angle   -> "<"
      | Close_Angle  -> ">"
      | Open_Square  -> "["
      | Close_Square -> "]"
      | Colon        -> ":"
      | Arrow        -> "->"
      | Identifier s -> "\"" ^ s ^ "\""
      | Int i        -> Int.to_string i
      | Float f      -> Float.to_string f
      | Def          -> "'def'"
      | Transform    -> "'transform'"
      | Renderer     -> "'renderer'"
      | Fun          -> "'fun'"
      | Let          -> "'let'"

  end

exception SyntaxError of string
module My_Lex = Lex.Make_Lexer(Generator_Token)
open Generator_Token

let peek (inquiry : t) (stream : t Stream.t) =
  let next = Stream.peek stream in
  match next with
  | Some next_token ->
     equal_kind inquiry next_token
  | _ -> false

let consume (inquiry : t) (stream : t Stream.t) =
  if peek inquiry stream
  then (Stream.junk stream; true)
  else false

let dummy_identifier = Identifier "dummy identifier"
let consume_identifier (stream : t Stream.t) =
  let next = Stream.peek stream in
  if consume dummy_identifier stream then
    match next with Some (Identifier s) -> Some s
                  | _ -> None
  else None

let expect_message (expectation : t) (stream : t Stream.t) =
  SyntaxError (match Stream.peek stream with
               | Some tkn ->
                  "expected " ^ to_string expectation
                  ^ " but encountered " ^ to_string tkn
                  ^ " instead."
               | None ->
                  "expected " ^ to_string expectation
                  ^ " but the file ended.")

let expect (expectation : t) (stream : t Stream.t) =
  if consume expectation stream then ()
  else raise (expect_message expectation stream)

let parse_vector_type (s : string) =
  let vect_re  = Str.regexp "vector\\([0-9]+\\)" in
  if Str.string_match vect_re s 0 then
    let card = Str.matched_group 1 s in
    AST.TyVector (int_of_string card)
  else
    raise (SyntaxError (s ^ " is not a valid type"))

exception Empty
let split_last (lst : 'a list) =
  let rec f = fun (head : 'a list) (tail : 'a list) ->
    match head, tail with
    | [last], ts -> ts, last
    | h::hs, ts -> f hs (h::ts)
    | [], _ -> raise Empty in
  f lst []

let rec parse_type (stream : t Stream.t) =
  let first = parse_type_term stream in
  let rec collect_terms = fun () ->
    if consume Arrow stream then
      let next = parse_type_term stream in
      next::(collect_terms ())
    else [] in
  match collect_terms () with
  | [] -> first
  | ts -> let formal_ts, ret_t = split_last (first::ts) in
          let formal_ts = List.rev formal_ts in
          AST.TyFun (formal_ts, ret_t)
  
and parse_type_term (stream : t Stream.t) =
  if consume Open_Paren stream then
    let inside = parse_type stream in
    expect Close_Paren stream; inside
  else if consume Open_Square stream then
    let inside = parse_type stream in
    expect Close_Square stream;
    AST.TyList inside
  else match consume_identifier stream with
       | Some "int" -> AST.TyInt
       | Some str -> parse_vector_type str
       | None -> raise (expect_message (Identifier "type") stream)

let parse_typed_id_list (stream : t Stream.t) =
  expect Open_Paren stream;
  let rec f = fun () ->
    if consume Close_Paren stream then []
    else let id = match consume_identifier stream with
           | Some str -> str
           | None -> raise (expect_message dummy_identifier stream) in
         expect Colon stream;
         let ty = parse_type stream in
         (id, ty)::(f ()) in
  f ()

let parse_vector (stream : t Stream.t) =
  let rec f = fun () ->
    if consume Close_Angle stream then []
    else match Stream.peek stream with
         | Some (Float n) -> Stream.junk stream; n::(f ())
         | Some (Int n) -> Stream.junk stream; (Float.of_int n)::(f ())
         | _ -> raise (expect_message (Float 0.) stream) in
  let elems = f () in
  Array.of_list elems

let rec parse_sexp (stream : t Stream.t) =
  if consume Open_Paren stream then
    if consume Fun stream then parse_abs stream
    else if consume Let stream then parse_let stream
    else parse_app stream
  else if consume Open_Square stream then parse_list stream
  else if consume Open_Angle stream then AST.Const (AST.Vector (parse_vector stream))
  else match Stream.peek stream with
       | Some (Identifier nm) -> Stream.junk stream; AST.Var nm
       | Some (Int n) -> Stream.junk stream; AST.Const (AST.Int n)
       | Some other ->
          let msg = "Unexpected " ^ to_string other ^ " in sexp" in
          raise (SyntaxError msg)
       | None ->
          let msg = "End of file reached while parsing sexp" in
          raise (SyntaxError msg)

and parse_app (stream : t Stream.t) =
  let rec f = fun () ->
    if consume Close_Paren stream then []
    else let e = parse_sexp stream in
         e::(f ()) in
  let elems = f () in
  match elems with
  | first::rest -> AST.App (first, rest)
  | [] -> let msg = "Empty application" in
          raise (SyntaxError msg)

and parse_abs (stream : t Stream.t) =
  let formals = parse_typed_id_list stream in
  let body = parse_sexp stream in
  expect Close_Paren stream;
  AST.Abs (formals, body)

and parse_list (stream : t Stream.t) =
  let rec f = fun () ->
    if consume Close_Square stream then []
    else let elem = parse_sexp stream in
         elem::(f ()) in
  AST.ListLiteral (f ())

and parse_let (stream : t Stream.t) =
  expect Open_Paren stream;
  let rec parse_bindings = fun () ->
    if consume Close_Paren stream then []
    else begin
        expect Open_Paren stream;
        let nm = match consume_identifier stream with
          | Some str -> str
          | None -> raise (expect_message dummy_identifier stream) in
        expect Colon stream;
        let ty = parse_type stream in
        let body = parse_sexp stream in
        expect Close_Paren stream;
        (nm, ty, body)::(parse_bindings ())
      end in
  let bindings = parse_bindings () in
  let body = parse_sexp stream in
  AST.Let (bindings, body)

let parse_decl (stream : t Stream.t) =
  match consume_identifier stream with
  | Some nm -> let body = parse_sexp stream in
               AST.Decl (nm, body)
  | None -> raise (expect_message dummy_identifier stream)

let parse_function_body (stream : t Stream.t) =
  match consume_identifier stream with
  | Some nm ->
     let formals = parse_typed_id_list stream in
     let body = parse_sexp stream in
     nm, formals, body
  | None -> raise (expect_message dummy_identifier stream)

let parse_def (stream : t Stream.t) =
  match Stream.npeek 2 stream with
  | [Open_Paren; Def      ] ->
     expect Open_Paren stream;
     expect Def stream;
     let res = parse_decl stream in
     expect Close_Paren stream; res
  | [Open_Paren; Transform] ->
     expect Open_Paren stream;
     expect Transform stream;
     let nm, formals, body = parse_function_body stream in
     expect Close_Paren stream;
     AST.Transform (nm, formals, body)
  | [Open_Paren; Renderer ] ->
     expect Open_Paren stream;
     expect Renderer stream;
     let nm, formals, body = parse_function_body stream in
     expect Close_Paren stream;
     AST.Renderer (nm, formals, body)
  | [Open_Paren;         _] ->
     let sexp = parse_sexp stream in
     AST.Decl ("it", sexp)
  | [tkn; _] | [tkn] -> let msg = to_string tkn ^ " is not allowed "
                                  ^ "in the toplevel" in
                        raise (SyntaxError msg)
  | _ -> raise (SyntaxError "Unexpected token in toplevel")

let rec parse_file (stream : t Stream.t) =
  if peek Open_Paren stream then
    let next_def = parse_def stream in
    next_def::(parse_file stream)
  else []

let parse (stream : Char.t Stream.t) =
  let token_stream = My_Lex.create stream in
  parse_file token_stream
