type vec = Vec2.t

type control_point = {
    point : Point.t;
    before : Point.t;
    after : Point.t;
  }
;;

type t = control_point list
;;

type path = Vec2.t list
;;

let clamp (s : float) (min, max : float * float) =
  if s < min then min
  else if s > max then max
  else s
;;
  
let lerp (a : vec) (b : vec) (s : float) =
  let s = clamp s (0., 1.) in
  let d = Vec2.sub b a in
  Vec2.add a (Vec2.scale d s)
;;

let qubic_bezier (a : vec) (b : vec) (c : vec) (d : vec) (s : float) =
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
  Array.init length (fun n ->
      d *. (Float.of_int (length - n)) -. d)
;;

let create (curve : t) =
  let rec f (curve : t) =
    match curve with
    | pa::pb::rest ->
       let a = Vec2.of_point pa.point
       and b = Vec2.of_point pa.after
       and c = Vec2.of_point pb.before
       and d = Vec2.of_point pb.point in
       let n_points = max 2 (Float.to_int (Vec2.distance a d *. 0.2)) in
       let samples = Array.to_list (linspace n_points) in
       let path = List.map (qubic_bezier a b c d) samples in
       path::(f (pb::rest))
    | _ -> [] in
  List.concat (f curve)
;;

let rec normals (path : path) =
  match path with
  | [] | [_] -> []
  | a::b::rest ->
     let dx, dy = Vec2.sub b a in
     let cross = dy, -.dx in
     (Vec2.norm cross)::(normals (b::rest))
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
  let rec f (last : vec) =
    function
    | [] -> [final]
    | p::rest ->
       let miter = Vec2.norm (Vec2.add p last) in
       let length = 1. /. (Vec2.dot last miter) in
       (Vec2.scale miter length)::(f p rest) in
  match ns with
  | fst::rest -> fst::(f fst rest)
  | [] -> raise Empty
;;

