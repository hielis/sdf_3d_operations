open Mesh
open Kernel

module Box : sig
  type 'a box = {x : 'a * 'a ; y : 'a * 'a ; z : 'a * 'a}
  val box : ('a * 'a * 'a) -> ('a * 'a * 'a) -> 'a box
  val vertices_list : 'a box -> ('a * 'a * 'a) list
end

module SdfRenderMaker : sig
  val export_to_obj : Mesh.mesh -> string
  val render_a_mesh : float -> Field.t -> (int * int * int) -> float Box.box -> Mesh.mesh
end
