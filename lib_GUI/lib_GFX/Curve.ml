type pnt = Point.t
;;

type control_point = {
    point : pnt;
    before : pnt;
    after : pnt;
  }
;;

type t = control_point list
;;

type path = Point.t list
;;

let clamp (s : float) (min, max : float * float) =
  if s < min then min
  else if s > max then max
  else s
;;
  
let lerp (a : pnt) (b : pnt) (s : float) =
  let s = clamp s (0., 1.) in
  let d = Point.sub b a in
  Point.add a (Point.scale d s)
;;

let qubic_bezier (a : pnt) (b : pnt) (c : pnt) (d : pnt) (s : float) =
  let ab = lerp a b s in
  let bc = lerp b c s in
  let cd = lerp c d s in
  let abc = lerp ab bc s in
  let bcd = lerp bc cd s in
  lerp abc bcd s
;;

(* returns 'length' floats linearly spaced
 * between 0. and 1., inclusive of both
 * 0. and 1. *)
let linspace (length : int) =
  let d = 1. /. (Float.of_int (length - 1)) in
  let rec f (n : int) =
    if n = 0 then []
    else (d *. (Float.of_int (length - n)))::f(n - 1) in
  f length
;;

let create (curve : t) =
  let rec f (curve : t) =
    match curve with
    | pa::pb::rest ->
       let samples = linspace 20 in
       let a = pa.point
       and b = pa.after
       and c = pb.before
       and d = pb.point in
       let path = List.map (qubic_bezier a b c d) samples in
       path::(f (pb::rest))
    | _ -> [] in
  List.concat (f curve)
;;

let rec normals (path : path) =
  match path with
  | [] | [_] -> []
  | a::b::rest ->
     let dx, dy = Point.sub b a in
     let cross = dy, -dx in
     (Point.norm cross)::(normals (b::rest))
;;

exception Empty
let rec last = function
  | [] -> raise Empty
  | [last] -> last
  | _::rest -> last rest
;;

let bisectors (path : path) =
  let ns = normals path in
  let final = last ns in
  let rec f (last : pnt) =
    function
    | [] -> [final]
    | p::rest ->
       let miter = Point.norm (Point.add p last) in
       let length = 1. /. (Float.of_int (Point.dot last miter)) in
       (Point.scale miter length)::(f p rest) in
  match ns with
  | fst::rest -> fst::(f fst rest)
  | [] -> raise Empty
;;

