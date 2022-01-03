module type TOKEN =
  sig
    type t

    type response =
      | Done of t
      | If of Char.t * response * response
      | Until of (Char.t -> bool) * (string -> t) (* inclusive *)
      | While of (Char.t -> bool) * (string -> t) (* exclusive *)    
      | Nothing
    val of_chars : Char.t -> response
    val of_int   : int -> t
    val of_float : float -> t
  end

module type LEXER =
  sig
    type t
    type tkn
    
    val create : Char.t Stream.t -> tkn Stream.t
  end

module Make_Lexer(Token : TOKEN) : (LEXER with type tkn = Token.t) =
  struct
    type tkn = Token.t
    type t = {
        stream : char Stream.t;
        curr_str : string;
        line : int;
        col : int }

    let consume (lexer : t ref) =
      let c = Stream.next !lexer.stream in
      lexer := { !lexer with curr_str = !lexer.curr_str ^ Char.escaped c;
                             line = !lexer.line + (if c = '\n' then 1 else 0);
                             col = if c = '\n' then 0 else !lexer.col + 1 };
      c
    
    let consume_while (pred : Char.t -> bool) (lexer : t ref) =
      let rec f lexer =
        let c = Stream.peek !lexer.stream
        in match c with
           | Some c when pred c ->
              (ignore (consume lexer);
               f lexer)
           | _ -> ()
      in f lexer

    let consume_until (pred : Char.t -> bool) (lexer : t ref) =
      consume_while (fun c -> not (pred c)) lexer;
      ignore (consume lexer)
    
    (* This function transparently passes an optional through
     * when the input token exists and only executes the 
     * "thunk" if the input token is None *)
    let do_if_none (thunk : unit -> tkn option) =
      function None -> thunk ()
             | Some c -> Some c
    
    let is_whitespace (c : Char.t) =
      (c = ' ' || c = '\n' || c = '\t' || c = '\r')

    let is_numeric (c : Char.t) =
      (c >= '0' && c <= '9')

    let number (lexer : t ref) =
      let stream = !lexer.stream in
      consume_while is_numeric lexer;
      let int_part_str = !lexer.curr_str in
      let has_dot = match Stream.peek stream
        with Some '.' -> true
           | _ -> false in
      let possible_float_part_str =
        if has_dot then
          (ignore (consume lexer); (* consume '.' *)
           consume_while is_numeric lexer;
           let str = !lexer.curr_str in
           Some str)
        else None in
      match (int_part_str, possible_float_part_str)
      with (si, None) -> (try Some (Token.of_int (int_of_string si))
                          with _ -> None)
         | (_,  Some sf) -> (try Some (Token.of_float (float_of_string sf))
                             with _ -> None)

    let dot_number (lexer : t ref) =
      consume_while is_numeric lexer;
      let float_part_str = !lexer.curr_str in
      try Some (Token.of_float (float_of_string float_part_str))
      with _ -> None

    let peek_or_null (lexer : t ref) =
      match Stream.peek !lexer.stream with
      | Some c -> c
      | None -> Char.chr 0

    let consume_whitespace_and_comments (lexer : t ref) =
      begin
        consume_while is_whitespace lexer;
        if (peek_or_null lexer) = '#' then
          consume_while (fun c -> not (c = '\n')) lexer;
        consume_while is_whitespace lexer
      end
    
    let next_token (lexer : t ref) =
      fun _ ->
      consume_whitespace_and_comments lexer;
      lexer := { !lexer with curr_str = "" };
      let c = try consume lexer with
                Stream.Failure -> Char.chr 0 in
      let tkn =
        let rec unroll = fun e ->
          match e with
          | Token.Done tkn -> Some tkn
          | Token.Nothing -> None
          | Token.If (target, true_e, false_e) ->
             let next_c = peek_or_null lexer in
             if target = next_c then
               (ignore (consume lexer);
                unroll true_e)
             else
               unroll false_e
          | Token.Until (pred, finalizer) ->
             consume_until pred lexer;
             Some (finalizer !lexer.curr_str)
          | Token.While (pred, finalizer) ->
             consume_while pred lexer;
             Some (finalizer !lexer.curr_str) in
        unroll (Token.of_chars c) in
      let tkn = do_if_none
                  (fun () ->
                    match c with
                    | '-' -> number lexer
                    | '.' -> dot_number lexer
                    | _ -> if is_numeric c then number lexer
                           else None) tkn in
      tkn
    
    let create (stream : char Stream.t) =
      let lexer =
        ref {
            stream; curr_str = "";
            line = 0; col = 0 } in
      Stream.from (next_token lexer)
  end

(* <><><><><><><><><><><><><><><><><><><><><><><><>  Tests <><><><><> *)

let%test_module _ =
  (module struct
     module Sexp_Token =
       struct
         type t =
           | OPEN_PAREN
           | CLOSE_PAREN
           | UNIT
           | INT of int
           | FLOAT of float
           | STRING of string
         
         type response = Done of t
                       | If of Char.t * response * response
                       | Until of (Char.t -> bool) * (string -> t) (* inclusive *)
                       | While of (Char.t -> bool) * (string -> t) (* exclusive *)
                       | Nothing
         
         let of_chars =
           function '(' -> If (')',
                               Done UNIT,
                               Done OPEN_PAREN)
                  | ')' -> Done CLOSE_PAREN
                  | '"' -> Until ((fun c -> c = '"'),
                                  (fun str -> STRING str))
                  | _ -> Nothing
         
         let of_int (i : int) = INT i
         let of_float (f : float) = FLOAT f
         let to_string = function
           | OPEN_PAREN -> "("
           | CLOSE_PAREN -> ")"
           | UNIT -> "(unit)"
           | INT i -> Int.to_string i
           | FLOAT f -> Float.to_string f
           | STRING s -> s
       end

     module MyLex = Make_Lexer(Sexp_Token)

     let rec check_tokens (stream : Sexp_Token.t Stream.t) =
       function
         t::ts ->
          let target_str = Sexp_Token.to_string t in
          let check_str = Sexp_Token.to_string (Stream.next stream) in
          if String.compare target_str check_str = 0
          then
            check_tokens stream ts
          else
            (print_endline (target_str ^ " != " ^ check_str);
             false)
       | [] -> true

     let check_string (str : string) (tokens : Sexp_Token.t list) =
       let lexer = MyLex.create (Stream.of_string str) in
       check_tokens lexer tokens

     let%test _ = check_string "1 1. 1.0 .1 -1 -1. -1.0"
                    [Sexp_Token.INT 1;
                     Sexp_Token.FLOAT 1.0;
                     Sexp_Token.FLOAT 1.0;
                     Sexp_Token.FLOAT 0.1;
                     Sexp_Token.INT (-1);
                     Sexp_Token.FLOAT (-1.0);
                     Sexp_Token.FLOAT (-1.0)]

     let%test _ = check_string "(1 .2 (3 ()) \"my string\")"
                    [Sexp_Token.OPEN_PAREN;
                     Sexp_Token.INT 1;
                     Sexp_Token.FLOAT 0.2;
                     Sexp_Token.OPEN_PAREN;
                     Sexp_Token.INT 3;
                     Sexp_Token.UNIT;
                     Sexp_Token.CLOSE_PAREN;
                     Sexp_Token.STRING "\"my string\"";
                     Sexp_Token.CLOSE_PAREN]
   end)
