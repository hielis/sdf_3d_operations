open Kernel


module Shape : sig

  val sphere : (float * float * float) -> float -> Field.t
  val plane : (float * float * float) -> (float * float * float) -> float Box.box  -> Field.t
  val box : float Box.box -> Field.t

end = struct

  let sphere origin radius =
    let ox, oy, oz = origin in
    let sphere_func x y z = (x -. ox) *. (x -. ox) +. (y -. oy) *. (y -. oy) +. (z -. oz) *. (z -. oz) -. radius *. radius in
    let boundaries = Box.box (ox -. radius , oy -. radius , oz -. radius) (ox +. radius , oy +. radius , oz +. radius) in
    Field.field sphere_func boundaries;;

  let plane (nx, ny, nz) (ox, oy, oz) box =
    let func x y z = (x -. ox) *. nx +.  (y -. oy) *. ny +. (z -. oz) *. nz in
    Field.field func box
  ;;

  let box b =
    let min a b = if (a > b) then b else a in
    let xmin, xmax = b.Box.x and ymin, ymax = b.Box.y and zmin, zmax = b.Box.z in
    let func x y z = min (min (min (x -. xmin) (xmax -. x)) (min (y -. ymin) (ymax -. y))) (min (z -. zmin) (zmax -. z)) in
    Field.field func b
  ;;

end
