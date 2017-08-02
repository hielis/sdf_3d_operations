open Kernel


module Shape : sig

  val sphere : (float * float * float) -> float -> Field.t
  val plane : (float * float * float) -> (float * float * float) -> float Box.box  -> Field.t
  val box : float Box.box -> Field.t

end
