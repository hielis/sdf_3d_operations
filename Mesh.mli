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

module type MeshType = sig
  type v
  type triangle
  type mesh
  val get_normal : triangle -> v
  val get_vertices : triangle -> v * v * v
  val get_triangles_list : mesh -> triangle list
  val get_vertices_list : mesh -> v list
  val get_normals_list : mesh -> v list
end
module MeshMaker = functor (V : VectorType) -> functor (S : SetType) -> sig
  type v
  type triangle
  type mesh
  val get_normal : triangle -> v
  val get_vertices : triangle -> v * v * v
  val get_triangles_list : mesh -> triangle list
  val get_vertices_list : mesh -> triangle list
  val get_normals_list : mesh -> v list
end

module MeshMake = functor (V:VectorType) -> MeshMaker (Vector) (Set.Make(Vector))
module Mesh : MeshType = MeshMake(Vector)
