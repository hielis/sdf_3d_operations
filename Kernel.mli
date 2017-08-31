
module Box : sig
  type 'a box = {x : 'a * 'a ; y : 'a * 'a ; z : 'a * 'a}
  val box : ('a * 'a * 'a) -> ('a * 'a * 'a) -> 'a box
  val vertices_list : 'a box -> ('a * 'a * 'a) list
  val max_box : 'a box -> 'a box -> 'a box
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

module Field :
sig
  type v = float
  type func
  type bound
  type t = func * bound
  type grid = v array * (int * int * int) * (float * float * float) * float
  val eval : t -> float -> float -> float -> v
  val boundaries : t -> float Box.box
  val field : (float -> float -> float -> v) -> float Box.box -> t
  val interpolate_field : grid -> t
  val use_a_function_left :
    (v -> v) -> (float Box.box -> float Box.box) -> t -> t
  val use_a_function_right :
    (float * float * float -> float * float * float) ->
    (float Box.box -> float Box.box) -> t -> t
  val use_a_binary_op :
    (v -> v -> v) ->
    (float Box.box -> float Box.box -> float Box.box) -> t -> t -> t
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

module FieldOperation :
sig
  val use_a_function :
    (Field.v -> Field.v) ->
    (float Box.box -> float Box.box) -> Field.t -> Field.t
  val use_a_binary_op :
    (float -> float -> float) ->
    (float Box.box -> float Box.box -> float Box.box) ->
    Field.t -> Field.t -> Field.t
  val translate : float * float * float -> Field.t -> Field.t
  val rotate : float -> float * float * float -> Field.t -> Field.t
  val scale : Field.v * Field.v * Field.v -> Field.t -> Field.t
  val union : Field.t -> Field.t -> Field.t
  val smooth_union : float -> Field.t -> Field.t -> Field.t
  val intersection : Field.t -> Field.t -> Field.t
  val substraction : Field.t -> Field.t -> Field.t
  val morph : float -> Field.t -> Field.t -> Field.t
end
