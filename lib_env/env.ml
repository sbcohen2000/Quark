exception Unequal_Length
exception Not_Found

let rec zip (a_lst : 'a list) (b_lst : 'b list) =
  match a_lst, b_lst with
  | ([], []) -> []
  | (a::rem_as, b::rem_bs) -> (a, b)::(zip rem_as rem_bs)
  | _ -> raise Unequal_Length

let rec zip3 (a_lst : 'a list) (b_lst : 'b list) (c_lst : 'c list) =
  match a_lst, b_lst, c_lst with
  | ([], [], []) -> []
  | (a::rem_as, b::rem_bs, c::rem_cs) -> (a, b, c)::(zip3 rem_as rem_bs rem_cs)
  | _ -> raise Unequal_Length

let rec unzip (list : ('a * 'b) list) =
  match list with
  | [] -> ([], [])
  | (a, b)::rest ->
     let rem_as, rem_bs = unzip rest in
     (a::rem_as, b::rem_bs)

let rec unzip3 (list : ('a * 'b * 'c) list) =
  match list with
  | [] -> ([], [], [])
  | (a, b, c)::rest ->
     let rem_as, rem_bs, rem_cs = unzip3 rest in
     (a::rem_as, b::rem_bs, c::rem_cs)

let rec find_opt (query : string) = function
  | [] -> None
  | ((key, value)::_) when key = query -> Some value
  | (_::rest) -> find_opt query rest

let rec find (query : string) = function
  | [] -> raise Not_Found
  | ((key, value)::_) when key = query -> value
  | (_::rest) -> find query rest;;

(* <><><><><><><><><><><><><><><><><><><><><><><><>  Tests <><><><><> *)

let%test _ = (List.length (zip [] [])) = 0
let%test _ = (zip [1; 2; 3] [4; 5; 6]) = [(1, 4); (2, 5); (3, 6)]

let%test _ = unzip [] = ([], [])
let%test _ = unzip [(1, 4); (2, 5); (3, 6)] = ([1; 2; 3], [4; 5; 6])

let%test _ = find_opt "a" [("a", 1); ("b", 2)] = Some 1
let%test _ = find_opt "a" [("a", 1); ("a", 2)] = Some 1
let%test _ = find_opt "c" [("a", 1); ("b", 2)] = None
