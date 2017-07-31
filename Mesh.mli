module type VectorType = sig
  type vect
  type t = vect * int
  val vect : float -> float -> float -> vect
  val get_x : vect -> float
  val get_y : vect -> float
  val t : vect -> int -> t
  val get_i : t -> int
  val get_z : vect -> float
  val scal_mult : float -> vect -> vect
  val dot_product : vect -> vect -> float
  val norm : vect -> float
  val wedge_product : vect -> vect -> vect
  val add : vect -> vect -> vect
  val sub : vect -> vect -> vect
  val equals : vect -> vect -> bool
  val compare : t -> t -> int
end

module Vector : VectorType


module type MeshType = functor (V : VectorType) ->
    sig
      type triangle = V.vect * V.vect * V.vect
      type face = int * int * int
      type mesh = V.vect array * face list
      exception Invalid_triangle
      val face_to_triangle : 'a array * 'b -> int * int * int -> 'a * 'a * 'a
      val triangle : 'a -> 'b -> 'c -> 'a * 'b * 'c
      val get_vertices_of_triangle : 'a * 'b * 'c -> 'a * 'b * 'c
(*      val get_vertices_of_face :
        'a array * 'b -> int * int * int -> 'a * 'a * 'a
*) 
     val get_triangles_list :
        'a array * (int * int * int) list -> ('a * 'a * 'a) list
      val mesh : (V.vect * V.vect * V.vect) list -> V.vect array * face list
      val get_vertices_array : 'a * 'b -> 'a
      val get_faces_list : 'a * 'b -> 'b
      val print_triangle : V.vect * V.vect * V.vect -> string
    end

module MeshMaker : MeshType


module Mesh :
    sig
      type triangle = Vector.vect * Vector.vect * Vector.vect
      type face = int * int * int
      type mesh = Vector.vect array * face list
      exception Invalid_triangle
      val face_to_triangle : 'a array * 'b -> int * int * int -> 'a * 'a * 'a
      val triangle : 'a -> 'b -> 'c -> 'a * 'b * 'c
      val get_vertices_of_triangle : 'a * 'b * 'c -> 'a * 'b * 'c
(* 
     val get_vertices_of_face :
        'a array * 'b -> int * int * int -> 'a * 'a * 'a
 *)  
    val get_triangles_list :
        'a array * (int * int * int) list -> ('a * 'a * 'a) list
      val mesh : (Vector.vect * Vector.vect * Vector.vect) list -> Vector.vect array * face list
      val get_vertices_array : 'a * 'b -> 'a
      val get_faces_list : 'a * 'b -> 'b
      val print_triangle : Vector.vect * Vector.vect * Vector.vect -> string
    end


