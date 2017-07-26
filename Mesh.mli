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

module MeshMake : MeshType

module Mesh : sig
    type triangle = MeshMake(Vector).triangle
    type mesh = MeshMake(Vector).mesh
    val triangle : Vector.vect -> Vector.vect -> Vector.vect -> triangle
    val mesh : triangle list -> mesh
    val get_normal : triangle -> Vector.vect
    val get_vertices : triangle -> Vector.vect * Vector.vect * Vector.vect
    val get_triangles_list : mesh -> triangle list
    val get_vertices_list : mesh -> Vector.vect list
    val get_normals_list : mesh -> Vector.vect list
  end
