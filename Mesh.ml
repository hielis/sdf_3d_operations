module type VectorType = sig
  type vect
  type t = vect
  val vect : float -> float -> float -> vect
  val get_x : vect -> float
  val get_y : vect -> float
  val get_z : vect -> float
  val scal_mult : float -> vect -> vect
  val dot_product : vect -> vect -> float
  val norm : vect -> float
  val wedge_product : vect -> vect -> vect
  val add : vect -> vect -> vect
  val equals : vect -> vect -> bool
  val compare : vect -> vect -> int
end


module Vector : sig
  type vect
  type t = vect
  val vect : float -> float -> float -> vect
  val get_x : vect -> float
  val get_y : vect -> float
  val get_z : vect -> float
  val scal_mult : float -> vect -> vect
  val dot_product : vect -> vect -> float
  val norm : vect -> float
  val wedge_product : vect -> vect -> vect
  val add : vect -> vect -> vect
  val sub : vect -> vect -> vect
  val equals : vect -> vect -> bool
  val compare : vect -> vect -> int
end = struct
  type vect = float * float * float
  type t = vect
  let vect x y z = (x, y, z);;
  let get_x = function (x, y, z) -> x;;
  let get_y = function (x, y, z) -> y;;
  let get_z = function (x, y, z) -> z;;
  let scal_mult sc = function (x, y, z) -> (sc *. x, sc *. y, sc *. z);;
  let dot_product v1 v2 =
    let x, y, z = v1 and xp, yp, zp = v2 in
    (x *. xp +. y *. yp +. z *. zp);;
  let norm x = sqrt (dot_product x x);;
  let wedge_product v1 v2 =
    let x, y, z = v1 and xp, yp, zp = v2 in
    (y *. zp -. yp *.z, x *. zp -. xp *.z, x *. yp -. y *.xp);;
  let add v1 v2 =
    let x, y, z = v1 and xp, yp, zp = v2 in
    (x +. xp, y +. yp, z +. zp);;
  let sub v1 v2 = add v1 (scal_mult (-.1.0) v2);;
  let equals v1 v2 =
    let x, y, z = sub v1 v2 in
    ((x == 0.0) && (y == 0.0) && (z == 0.0));;
  let compare v1 v2 =
    let v = sub v1 v2 in
    let x = get_x v and y = get_y v and z = get_z v in
    if (not (x == 0.0)) then int_of_float (x /. (abs_float x))
    else (if (not (y == 0.0)) then int_of_float (y /. (abs_float y))
          else int_of_float (z /. (abs_float z)));;
end

module type MeshType = functor  (V : VectorType) -> sig
  type triangle
  type mesh
  val mesh : triangle list -> mesh
  val triangle : V.vect -> V.vect -> V.vect -> triangle
  val mesh : triangle list -> mesh
  val get_normal : triangle -> V.vect
  val get_vertices : triangle -> V.vect * V.vect * V.vect
  val get_triangles_list : mesh -> triangle list
  val get_vertices_list : mesh -> V.vect list
  val get_normals_list : mesh -> V.vect list
end


module type SetType = sig
  type elt
  type t
  val add : elt -> t -> t
  val empty : t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end
module SetVector = Set.Make(Vector)

module MeshMaker = functor (V : VectorType) -> functor (S : SetType) -> struct
(*
                sig
                type v
                type triangle
                type mesh
                val get_normal : triangle -> v
                val get_vertices : triangle -> v * v * v
                val get_triangles_list : mesh -> triangle list
                val get_vertices_list : mesh -> triangle list
                val get_normals_list : mesh -> v list
              end = struct
*)
    type triangle = V.vect * V.vect * V.vect * V.vect
    type mesh = triangle list
    let mesh l = l;;
    let compute_normal v1 v2 v3 = V.vect 0.0 0.0 0.0;;
    let triangle v1 v2 v3 =
      let vn = compute_normal v1 v2 v3 in
      (v1, v2, v3, vn);;
    let get_normal ((a, b, c, d) : triangle) =  a;;
    let get_vertices = function (a, b, c, d) -> (b, c, d);;
    let get_triangles_list m =
      let rec aux acc = function [] -> [] | a::tl -> aux (a::acc) tl in
      aux [] m;;
    let get_vertices_list m =
      let rec aux s = function t::tl -> let a, b, c = get_vertices t in
                                    aux (S.add a (S.add b (S.add c s))) tl
                             | [] -> s
      in
      let rec to_list s = S.fold (List.cons) s [] in
      to_list (aux S.empty (get_triangles_list m));;
    let get_normals_list m = List.map (get_normal) (get_triangles_list m);;
  end

module MeshMake : MeshType = functor (V:VectorType) -> MeshMaker (V) (Set.Make(V))
module Mesh = MeshMake(Vector)
