let constant = 100000.0;;
module type SignedDistanceFunction = sig
  type v
  type func
  type bound
  type t
  val eval : t -> float -> float -> float -> v
  val boundaries : t -> float * float * float
  val field : func -> bound -> t
end

module Field : sig
  type v = float
  type func
  type bound
  type t = func * bound
  type grid = (v array) * (int * int * int) * (float * float * float) * float
  val eval : t -> (float -> float -> float -> v)
  val boundaries : t -> float * float * float
  val field : (float -> float -> float -> v) -> (float * float * float) -> t
  val interpolate_field : grid -> t
  val use_a_function_left : (v -> v) -> t -> t
  val use_a_function_right : ((float*float*float) -> (float*float*float)) -> ((float * float * float) -> (float * float * float)) -> t -> t
  val use_a_binary_op : (v -> v -> v) -> t -> t -> t
end = struct
  type v = float
  type func = float -> float -> float -> float
  type bound = float * float * float
  type t = (float -> float -> float -> float) * (float * float * float)
  type grid = (float array)*(int * int * int)*(float * float * float) * float

  let eval = function (f, (a, b, c)) ->
                      let r x y z = if (((abs_float x) > a) && ((abs_float y) > b) && ((abs_float z) > c)) then constant
                                    else (f x y z)
                      in
                      r;;

  let boundaries = function (f, b) -> b;;

  let field f b = (f, b);;

  let interpolate_field = function (a, (res_x, res_y, res_z), (o_x, o_y, o_z), d) ->
                                   let boundaries = (abs_float (d *. (float_of_int res_x) -. o_x), abs_float (d *. (float_of_int res_y) -. o_y), abs_float (d *. (float_of_int res_z) -. o_z))
                                   and max_a = res_x * res_y * res_z in
                                   let this_grid x y z = match (x + res_x * y + res_x * res_y *  z) with  p when (p > max_a) -> failwith "Pointer is Out of Bound"
                                     |n -> a.(n) in
                                   let f x y z =
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
                                     let i3 = [|hp *. i2.(0) +. i2.(2) *. y_d ;hp *. i2.(1) +. i2.(3) *. y_d|] in
                                     (i3.(0) *. (1.0 -. z_d) +. i3.(1) *. z_d)
                                   in
                                   field f boundaries
  ;;
  let use_a_function_left (f : v -> v) = function (a, b) -> let fp x  y  z = f (a x y z) in
                                             ((fp), b);;
  let use_a_function_right f fb = function (a, b) -> let bp = fb b in
                                             let fp x y z =
                                               let xp, yp, zp = f (x, y, z) in
                                               (a xp yp zp)
                                             in
                                             (fp, bp);;
  let max a b = if (a > b) then a else b;;
  let use_a_binary_op (op : v -> v -> v) (f1:t) (f2:t) = match (f1, f2) with (a,(x, y, z)),(ap, (xp, yp, zp)) -> let fp x y z = op (a x y z) (ap x y z) in
      (fp, (max xp x, max yp y, max z zp));;
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
end

