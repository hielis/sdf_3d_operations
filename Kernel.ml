let constant = 10000000.0;;
let max a b = if (a > b) then a else b;;
let min a b = if (b > a) then a else b;;


module Box : sig
  type 'a box = {x : 'a * 'a ; y : 'a * 'a ; z : 'a * 'a}
  val box : ('a * 'a * 'a) -> ('a * 'a * 'a) -> 'a box
  val vertices_list : 'a box -> ('a * 'a * 'a) list
  val max_box : 'a box -> 'a box -> 'a box
end  = struct
  type 'a box = {x : 'a * 'a ; y : 'a * 'a ; z : 'a * 'a}
  let vertices_list box =
    let (xmax, xmin), (ymax, ymin), (zmin, zmax) = box.x, box.y, box.z in
    [(xmin, ymin, zmin);(xmin, ymax, zmin); (xmax, ymax, zmin); (xmax, ymin, zmin);
    (xmin, ymin, zmax);(xmin, ymax, zmax); (xmax, ymax, zmax); (xmax, ymin, zmax)];;
 let box (a, b, c) (d, e, f) = {x = (a, d); y = (b,e); z = (c, f)};;
 let max_box b1 b2 =
   let (x1_min, x1_max), (x2_min, x2_max) = b1.x, b2.x
   and (y1_min, y1_max), (y2_min, y2_max) = b1.y, b2.y
   and (z1_min, z1_max), (z2_min, z2_max) = b1.z, b2.z in
   (box ((min x1_min x2_min), (min y1_min y2_min), (min z1_min z2_min)) ((max x1_max x2_max), (max y1_max y2_max), (max z1_max z2_max)))
;;
end

module type SignedDistanceFunction = sig
  type v
  type func
  type bound
  type t
  val eval : t -> float -> float -> float -> v
  val boundaries : t -> float Box.box
  val field : func -> bound -> t
end

module Field : sig
  type v = float
  type func
  type bound
  type t = func * bound
  type grid = (v array) * (int * int * int) * (float * float * float) * float
  val eval : t -> (float -> float -> float -> v)
  val boundaries : t -> float Box.box
  val field : (float -> float -> float -> v) -> (float Box.box) -> t
  val interpolate_field : grid -> t
  val use_a_function_left : (v -> v) -> ((float Box.box) -> (float Box.box)) -> t -> t
  val use_a_function_right : ((float*float*float) -> (float*float*float)) -> ((float Box.box) -> (float Box.box)) -> t -> t
  val use_a_binary_op : (v -> v -> v) -> ((float Box.box) -> (float Box.box) -> (float Box.box)) -> t -> t -> t
end = struct
  include Box
  type v = float
  type func = float -> float -> float -> float
  type bound = float box
  type t = (float -> float -> float -> float) * (float box)
  type grid = (float array)*(int * int * int)*(float * float * float) * float

  let eval = function (f, b) ->
                      let xmin, xmax = b.x and ymin, ymax = b.y
                          and zmin, zmax = b.z in

                      let r x y z = if (((x > xmax) || (x < xmin)) && ((y > ymax) || (y < ymin)) && ((z > zmax) || (z < zmin))) then constant
                                    else (f x y z)
                      in
                      r
  ;;

  let boundaries = function (f, b) -> b;;

  let field f b = (f, b);;

  let interpolate_field =
    function (a, (res_x, res_y, res_z), (o_x, o_y, o_z), d) ->
             let boundaries = box (o_x, o_y, o_z) ((o_x +. d *. (float_of_int res_x)), (o_y +. d *. (float_of_int res_y)), (o_z +. d *. (float_of_int res_z)))
             and max_a = res_x * res_y * res_z - 1 in
             let this_grid x y z =
               match (x + res_x * y + res_x * res_y *  z) with
               |p when ((p > max_a) || (p < 0)) -> constant
               |n -> try (a.(n)) with Invalid_argument(s) ->
                       print_int (Array.length a);
                       print_string "|-- --|";
                       print_int n;
                       print_string "|--| |--|";
                       failwith s
             in
             let f x y z =
               let xmin, xmax = boundaries.x and ymin, ymax = boundaries.y
                   and zmin, zmax = boundaries.z in
               if (((x > xmax) || (x < xmin)) || ((y > ymax) || (y < ymin)) || ((z > zmax) || (z < zmin))) then (constant)
               else(
                 let x_p = int_of_float (floor ((x -. o_x) /. d))
                 and y_p = int_of_float (floor ((y -. o_y) /. d))
                 and z_p = int_of_float (floor ((z -. o_z) /. d)) in
                 let x_d = (x -. (o_x +. (float_of_int x_p) *. d)) /. d
                 and y_d = (y -. (o_y +. (float_of_int y_p) *. d)) /. d
                 and z_d = (z -. (o_z +. (float_of_int z_p) *. d)) /. d
                 and x_pp = x_p + 1
                 and y_pp = y_p + 1
                 and z_pp = z_p + 1 in
                 let i1  = [|this_grid x_p y_p z_p; this_grid x_pp y_p z_p; this_grid x_p y_pp z_p; this_grid x_p y_p z_pp; this_grid x_pp y_pp z_p; this_grid x_pp y_p z_pp; this_grid x_p y_pp z_pp; this_grid x_pp y_pp z_pp|] in
                 let h = (1.0 -. x_d) in
                 let i2 = [|i1.(0) *. h +. i1.(1) *. x_d; i1.(3) *. h +. i1.(5) *. x_d; i1.(2) *. h +. i1.(4) *. x_d; i1.(6) *. h +. i1.(7) *. x_d|] in
                 let hp = (1.0 -. y_d) in
                 let i3 = [|hp *. i2.(0) +. i2.(2) *. y_d; hp *. i2.(1) +. i2.(3) *. y_d|] in
                 (i3.(0) *. (1.0 -. z_d) +. i3.(1) *. z_d)
               )
             in
             field f boundaries
;;



(*Change*)
  let use_a_function_left (f : v -> v) fb = function (a, b) -> let fp x  y  z = f (a x y z) in
                                             ((fp), fb b);;






  let use_a_function_right f fb =
    function (a, b) -> let bp = fb b in
                       let fp x y z =
                         let xp, yp, zp = f (x, y, z) in
                         (a xp yp zp)
                       in
                       (fp, bp);;

  let max a b = if (a > b) then a else b;;

(*Change*)

let use_a_binary_op (op : v -> v -> v) fb (f1:t) (f2:t) = match (f1, f2) with (a,b),(ap, bp) -> let fp x y z = op (a x y z) (ap x y z) in
      (fp, fb b bp);;
end

module type SdfOperation = functor (M : SignedDistanceFunction) -> sig
                             val use_a_function : (M.v -> M.v) -> M.t -> M.t
                             val use_a_binary_op : (M.v -> M.v -> M.v) ->	M.t -> M.t
                             val translate : (float * float * float) ->  M.t -> M.t
                             val rotate : float -> (float * float * float) -> M.t -> M.t
                             val union : M.t -> M.t -> M.t
                             val smooth_union : float -> M.t -> M.t -> M.t
                             val morph : float -> M.t -> M.t -> M.t
                             val substraction : M.t -> M.t -> M.t
                             val intersection : M.t -> M.t -> M.t
                             val scale : (M.v * M.v * M.v) ->M.t -> M.t
end

module FieldOperation : sig
  val use_a_function : (Field.v -> Field.v) -> ((float Box.box) -> (float Box.box)) -> Field.t -> Field.t
  val use_a_binary_op : (float -> float -> float) -> (float Box.box -> float Box.box -> float Box.box) -> Field.t -> Field.t -> Field.t
  val translate : (float * float * float) -> Field.t -> Field.t
  val rotate : float -> (float * float * float) -> Field.t -> Field.t
  val scale : (Field.v * Field.v * Field.v) -> Field.t -> Field.t
  val union : Field.t -> Field.t -> Field.t
  val smooth_union : float -> Field.t -> Field.t -> Field.t
  val intersection : Field.t -> Field.t -> Field.t
  val substraction : Field.t -> Field.t -> Field.t
  val morph : float -> Field.t -> Field.t -> Field.t
end = struct
  let use_a_function = Field.use_a_function_left;;
  let use_a_binary_op = Field.use_a_binary_op;;
  let max a b = if (a > b) then a else b;;
  let min a b = if (a < b) then a else b;;
  let translate_func vector =
    let vx, vy, vz = vector in
    fun (x, y, z) -> (x -. vx, y -. vy, z -. vz);;

(*Change*)
  let modify_bound_translate vector =
    let vx, vy, vz = vector in
    (fun b -> let xmin, xmax = b.Box.x
              and ymin, ymax = b.Box.y
              and zmin, zmax = b.Box.z in
     Box.box (xmin +. vx, ymin +. vy, zmin +. vz) (xmax +. vx, ymax +. vy, zmax +. vz)
    )
  ;;



  let translate vector = Field.use_a_function_right (translate_func vector) (modify_bound_translate vector);;

  let rotate_func theta vector =
    let nx, ny, nz = vector in
    fun (x, y, z) ->(
      let d = (1.0 -. (cos  (-. theta))) *. (nx *. x +. ny *. y +. nz *. z) in
      (((cos (-. theta)) *. x +. d *. nx +. (sin (-. theta))) *. (y *. nz -. z *. ny),
       (cos (-. theta)) *. x +. d *. nx +. (sin (-. theta)) *. (y *. nz -. z *. ny),
       (cos (-. theta)) *. x +. d *. nx +. (sin (-. theta)) *. (y *. nz -. z *. ny)));;



(*Change*)
  let rotate_bounding_func theta vector =
    let r = (rotate_func (-. theta) vector) in
    (fun b -> let xmin, xmax = b.Box.x
              and ymin, ymax = b.Box.y
              and zmin, zmax = b.Box.z in
              let amin, bmin, cmin  = r (xmin, ymin, zmin)
              and amax, bmax, cmax  = r (xmax, ymax, zmax) in
              let dmin, dmax = min (min amin bmin) cmin, max (max amax bmax) cmax in
              Box.box (dmin, dmin, dmin) (dmax, dmax, dmax)
    )
  ;;



  let rotate theta vector = Field.use_a_function_right (translate_func vector) (rotate_bounding_func theta vector);;

  let scale_func lambda =
    let lx, ly, lz = lambda in
    fun (x, y, z) -> (x /. lx, y /. ly, z /. lz);;




(*Change*)
  let scale_bound (a, b, c) =
    let f = (scale_func (1.0 /. a, 1.0 /. b, 1.0 /. c)) in
    (fun v -> let xmin, xmax = v.Box.x
              and ymin, ymax = v.Box.y
              and zmin, zmax = v.Box.z in
              Box.box (f (xmin,ymin,zmin)) (f (xmax, ymax, zmax))
    )
  ;;






  let scale lambda = Field.use_a_function_right (scale_func lambda) (scale_bound lambda);;

  let union = Field.use_a_binary_op min Box.max_box;;
  let smooth_union factor =
      let smooth_func a b =
          if (abs_float (a) > 0.9 *. constant) then (min a b)
          else if (abs_float (b) > 0.9 *. constant) then (min a b)
          else if (abs_float (a -. b) > 0.2) then (min a b)
          else (
          let res = (exp (-.factor*.a)) +. (exp (-.factor*.b)) in
          (-. (log (res)) /. factor)
          )
          (* let c = a**factor in *)
          (* let d = b**factor in *)
          (* ((c*.d)/.(c+.d)) ** (1.0/.factor) *)
      in
      Field.use_a_binary_op smooth_func Box.max_box;;
  let substract_op a b = max a (-. b);;
  let intersection = Field.use_a_binary_op max Box.max_box;;
  let substraction = Field.use_a_binary_op substract_op Box.max_box;;
  let morph_op f = if ((f < 0.0) || (f > 1.0)) then failwith "Morph parameter too big" else (fun a b -> (f*.a +. (1.0 -. f) *. b))
  let morph f = Field.use_a_binary_op (morph_op f) Box.max_box;;

end
