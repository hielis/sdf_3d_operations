open Mesh
open Kernel

module SdfRenderMaker : sig
  val export_to_obj : Mesh.mesh -> string
  val render_a_mesh : float -> Field.t -> (int * int * int) -> float Box.box -> Mesh.mesh
  val render_a_mesh_fast : float -> Field.t -> (int * int * int) -> float Box.box -> Mesh.mesh
end
