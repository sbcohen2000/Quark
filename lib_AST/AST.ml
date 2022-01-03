type ty = TyFun of ty list * ty
        | TyVector of int
        | TyList of ty
        | TyInt
        | TyTop

type value = Vector of float Array.t
           | List of value list
           | Int of int
           | Closure of (string * exp) list * (string * value) list

and exp = Const of value
        | ListLiteral of exp list
        | App of exp * exp list
        | Abs of (string * ty) list * exp
        | Var of string
        | Let of (string * ty * exp) list * exp

type def = Transform of string * (string * ty) list * exp
         | Renderer of string * (string * ty) list * exp
         | Decl of string * exp

module Stringify =
  struct
    let rec value =
      function
      | Vector fs ->
         let strs = Array.map Float.to_string fs in
         "<" ^ String.concat " " (Array.to_list strs) ^ ">"
      | List vs ->
         let strs = List.map value vs in
         "[" ^ String.concat " " strs ^ "]"
      | Int i -> Int.to_string i
      | Closure _ -> "closure"

    let rec ty =
      function
      | TyFun (formal_ts, ret_t) ->
         let formal_strs = List.map ty formal_ts in
         let ret_t_str = ty ret_t in
         "(" ^ String.concat " " formal_strs ^ ") -> " ^ ret_t_str
      | TyVector n ->
         "vector" ^ Int.to_string n
      | TyList t ->
         "[" ^ ty t ^ "]"
      | TyInt -> "int"
      | TyTop -> "top"

    let binding (bindings : (string * ty) list) =
      let nms, tys = Env.unzip bindings in
      let ty_strs = List.map ty tys in
      let binding_strs = List.map2 (fun a b -> a ^ " : " ^ b) nms ty_strs in
      String.concat ", " binding_strs

    let rec tree (e : exp) (indent : string) =
      let (label, children) =
        match e with
        | Const v -> "const (" ^ value v ^ ")", []
        | ListLiteral es -> "list", es
        | App (e, es) -> "app", e::es
        | Abs (formals, e) ->
           let formal_str = binding formals in
           "fun (" ^ formal_str ^ ")", [e]
        | Var nm -> "var (" ^ nm ^ ")", []
        | Let (bdgs, e) ->
           let nms, tys, es = Env.unzip3 bdgs in
           let bindings = Env.zip nms tys in
           let binding_str = binding bindings in
           "let (" ^ binding_str ^ ")", e::es in
      let rec child_string = function
        | [] -> ""
        | [e] -> indent ^ "+-- " ^
                   tree e (indent ^ "    ")
        | e::es -> indent ^ "|-- " ^
                     tree e (indent ^ "|   ") ^
                       child_string es in
      label ^ "\n" ^ child_string children
  end
