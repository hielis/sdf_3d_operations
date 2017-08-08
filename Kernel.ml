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
  val use_a_function_left : (v -> v) -> t -> t
  val use_a_function_right : ((float*float*float) -> (float*float*float)) -> ((float * float * float) -> (float * float * float)) -> t -> t
  val use_a_binary_op : (v -> v -> v) -> t -> t -> t
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
  let use_a_function_left (f : v -> v) = function (a, b) -> let fp x  y  z = f (a x y z) in
                                             ((fp), b);;
  let use_a_function_right f fb =
    function (a, b) -> let (xm,xM), (ym,yM), (zm, zM) = b.x, b.y, b.z in
             let bp = box (f (xm, ym, zm)) (f (xM, yM, zM)) in
             let fp x y z =
               let xp, yp, zp = f (x, y, z) in
               (a xp yp zp)
             in
             (fp, bp);;
  let max a b = if (a > b) then a else b;;
  let use_a_binary_op (op : v -> v -> v) (f1:t) (f2:t) = match (f1, f2) with (a,b),(ap, bp) -> let fp x y z = op (a x y z) (ap x y z) in
      (fp, max_box b bp);;
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
  val use_a_function : (float -> float) -> Field.t -> Field.t
  val use_a_binary_op : (float -> float -> float) -> Field.t -> Field.t -> Field.t
  val translate : (float * float * float) -> Field.t -> Field.t
  val rotate : float -> (float * float * float) -> Field.t -> Field.t
  val scale : (Field.v * Field.v * Field.v) -> Field.t -> Field.t
  val union : Field.t -> Field.t -> Field.t
  val intersection : Field.t -> Field.t -> Field.t
  val substraction : Field.t -> Field.t -> Field.t
  val morph : float -> Field.t -> Field.t -> Field.t
  val repetition : int -> (float * float * float) -> Field.t -> Field.t
  val test : int -> Field.t -> Field.t
end = struct
  let use_a_function = Field.use_a_function_left;;
  let use_a_binary_op = Field.use_a_binary_op;;
  let max a b = if (a > b) then a else b;;
  let min a b = if (a < b) then a else b;;
  let translate_func vector =
    let vx, vy, vz = vector in
    fun (x, y, z) -> (x -. vx, y -. vy, z -. vz);;
  let modify_bound_translate vector =  let vx, vy, vz = vector in
    fun (x, y, z) -> (x +. (abs_float vx), y +. (abs_float vy), z +. (abs_float vz));;
  let translate vector = Field.use_a_function_right (translate_func vector) (modify_bound_translate vector);;

  let rotate_func theta vector =
    let nx, ny, nz = vector in
    fun (x, y, z) ->(
      let d = (1.0 -. (cos  (-. theta))) *. (nx *. x +. ny *. y +. nz *. z) in
      (((cos (-. theta)) *. x +. d *. nx +. (sin (-. theta))) *. (y *. nz -. z *. ny),
       (cos (-. theta)) *. x +. d *. nx +. (sin (-. theta)) *. (y *. nz -. z *. ny),
       (cos (-. theta)) *. x +. d *. nx +. (sin (-. theta)) *. (y *. nz -. z *. ny)));;
  let rotate_bounding_func theta vector =
    let r = (rotate_func (-. theta) vector) in
    fun (x, y, z) -> (
      let a, b, c  = r (x, y, z) in
      (max a x, max b y, max c z)
    );;

  let rotate theta vector = Field.use_a_function_right (translate_func vector) (rotate_bounding_func theta vector);;

  let scale_func lambda =
    let lx, ly, lz = lambda in
    fun (x, y, z) -> (x /. lx, y /. ly, z /. lz);;
  let scale_bound (a, b, c) = scale_func (1.0 /. a, 1.0 /. b, 1.0 /. c);;

  let scale lambda = Field.use_a_function_right (scale_func lambda) (scale_bound lambda);;
  let union = Field.use_a_binary_op min;;
  let substract_op a b = max a (-. b);;
  let intersection = Field.use_a_binary_op max;;
  let substraction = Field.use_a_binary_op substract_op;;
  let morph_op f = if ((f < 0.0) || (f > 1.0)) then failwith "Morph parameter too big" else (fun a b -> (f*.a +. (1.0 -. f) *. b))
  let morph f = Field.use_a_binary_op (morph_op f);;

  let repetition n v =
    let fmod (x, y, z) = match v with
      |(0., 0., 0.) -> (x, y, z)
      |(0., 0., zp) -> (x, y, z -. (floor (z/. zp)))
      |(0., yp, 0.) -> (x, y -. (floor (y /. yp))  ,z)
      |(xp, 0., 0.) -> (x -. (floor (x /. xp)), y, z)
      |(0., yp, zp) ->
        let k = min (floor (z/. zp)) (floor (y /. yp)) in
        (x, y -. k *. yp, z -. k *. zp)
      |(xp, 0., zp) ->
        let k = min (floor (z/. zp)) (floor (x /. xp)) in
        (x -. k *. xp, y, z -. k *. zp)
      |(xp, yp, 0.) ->
        let k = min (floor (x/. xp)) (floor (y /. yp)) in
        (x -. k *.xp, y -. k *. yp, z)
      |(xp, yp, zp) ->  let k = min (floor (z/. zp))  (min (floor (x/. xp)) (floor (y /. yp))) in
        (x -. k *.xp, y -. k *. yp,  z -. k *. zp)
    in
    let p = float_of_int n in
    let bound_func = scale_func (p, p, p) in
    Field.use_a_function_right (fmod) (bound_func)
  ;;

  let test n f =
    let b = Field.boundaries f in
    let xmin, xmax = b.Box.x
      and  ymin, ymax = b.Box.y
      and  zmin, zmax = b.Box.z in
    let ax = (xmax -. xmin) in
    let ay = (ymax -. ymin) in
    let az = (zmax -. zmin) in
    let fx x = (x -. xmin) /. ax in
    let fy y = (y -. ymin) /. ay in
    let fz z = (z -. zmin) /. az in
    let gx x = x *. ax +. xmin in
    let gy y = y *. ay +. ymin in
    let b_func (x, y, z) = (x, y, z) in
    let gz z = z *. az +. zmin in
    let boulanger_x (x,y) = match (fx x), (fy y) with
      |a, b when (a < 0.5) -> (gx (2. *. a), gy (y *. 0.5))
      |a, b -> (gx (2. *. a -. 1.), gy (0.5 *. (b +. 1.)))
    in
    let boulanger_y (y, z) = match (fy y), (fz z) with
      |a, b when (a < 0.5) -> (gy (2. *. a), gz (b *. 0.5))
      |a, b -> (gy (2. *. a -. 1.), gz (0.5 *. (b +. 1.)))
    in
    let boulanger_z (x, z) = match (fx x), (fz z) with
      |a, b when (a < 0.5) -> (gx (2. *. a), gz (b *. 0.5))
      |a, b -> (gx (2. *. a -. 1.), gz (0.5 *. (b +. 1.)))
    in
    let func (x, y, z) =
      let xp, yp = boulanger_x (x, y) in
      let ypp, zp = boulanger_y (yp, z) in
      let zpp, xpp = boulanger_z (xp, zp) in
      (xpp, ypp, zpp)
    in
    let rec loop acc = function 0 -> acc
                          | n ->
                             let h a = func (acc a) in
                             loop h (n - 1)
    in
    let transform = loop func n in
    Field.use_a_function_right (transform) (b_func) f
  ;;
end
