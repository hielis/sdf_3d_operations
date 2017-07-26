open Kernel
open Mesh
open Tables

module Box : sig
  type 'a box = {x : 'a * 'a ; y : 'a * 'a ; z : 'a * 'a}
  val box : ('a * 'a * 'a) -> ('a * 'a * 'a) -> 'a box
  val vertices_list : 'a box -> ('a * 'a * 'a) list
end  = struct
  type 'a box = {x : 'a * 'a ; y : 'a * 'a ; z : 'a * 'a}
  let vertices_list box =
    let (xmax, xmin), (ymax, ymin), (zmin, zmax) = box.x, box.y, box.z in
    [(xmin, ymin, zmin);(xmin, ymax, zmin); (xmax, ymax, zmin); (xmax, ymin, zmin);
    (xmin, ymin, zmax);(xmin, ymax, zmax); (xmax, ymax, zmax); (xmax, ymin, zmax)];;
 let box (a, b, c) (d, e, f) = {x = (a, d); y = (b,e); z = (c, f)};;
end

module SdfRenderMaker : sig
  val export_to_obj : Mesh.mesh -> string
  val render_a_mesh : float -> Field.t -> (int * int * int) -> float Box.box -> Mesh.mesh
end = struct
  let export_to_obj m : string =
    let aux v =
      let x, y, z = Vector.get_x v, Vector.get_y v, Vector.get_z v in
      (String.concat " " ["v" ; string_of_float x; string_of_float y; string_of_float z])
    in
    let aux1 (t : Mesh.triangle) =
      let v1, v2, v3 = Mesh.get_vertices t in
      (String.concat "\n" [aux v1; aux v2; aux v3])
    in
    String.concat "\n" (List.map aux1 (Mesh.get_triangles_list m))
  ;;
  let render_a_mesh iso f res box =
    let r_x, r_y, r_z = res in
    let step (a, d) r =
      (d -. a) /. (float_of_int r)
    in
    let h_x, h_y, h_z = step box.Box.x r_x, step box.Box.y r_y, step box.Box.z r_z in
    let (o_x,_), (o_y, _), (o_z, _) = box.Box.x, box.Box.y, box.Box.z in
    let rec get_grid acc i j k =
      let eval i j k = (Field.eval f ((float_of_int i) *.h_x +. o_x) ((float_of_int j) *.h_y +. o_y) ((float_of_int k) *.h_z +. o_z))
      in

      if (i == r_x) then get_grid ((eval i j k)::acc)  0 (j + 1) k
      else if (j == r_y) then get_grid ((eval i j k)::acc) 0 0 (k + 1)
      else if (k == r_z) then  (List.rev acc)
      else get_grid ((eval i j k)::acc) (i + 1) j  k
    in

    let callable_grid =
      let a = Array.of_list (get_grid [] 0 0 0) in
      (fun (w, b, c) -> a.(w + r_x * b + r_y * r_x * c))
    in

    let interpolate p1 p2 v1 v2 =
      Vector.add (p1) (Vector.scal_mult ((iso -. v1) /. (v2 -. v1)) (Vector.sub p2 p1))
    in

    let compute_cube_index cube =
      let rec aux i acc = function a::tl when  (a < iso) -> aux (i + 1) (acc + (1 lsl i)) tl
                                 | a::tl -> aux (i + 1) (acc) tl
                                 | [] -> acc in
      let rec aux2 acc = function a::tl -> aux2 ((callable_grid a)::acc) tl
                                | [] -> List.rev acc in
      aux 0 0 (aux2 [] (Box.vertices_list cube))
    in

    let to_vect (cx, cy, cz) = Vector.vect ((float_of_int cx) *.h_x +. o_x) ((float_of_int cy) *.h_y +. o_y) ((float_of_int cz) *.h_z +. o_z) in


    let compute_vertices_list cube_index vertices_list =
      let edge_index = Tables.edge_table.(cube_index) in
      let rec aux i acc m = function 0 -> List.rev acc
                                    |n when (((1 lsl i) land m) == 1)  ->
                                      let e1, e2 = Tables.edge_list.(i) in
                                      let p1, p2 = List.nth vertices_list e1, List.nth vertices_list e2 in
                                      let q1, q2 = to_vect p1, to_vect p2 in
                                      let v1, v2 = callable_grid p1, callable_grid p2 in
                                      aux (i + 1) ((interpolate q1 q2 v1 v2)::acc) m (n -1)
                                    |n -> aux (i + 1) ((Vector.vect 0.0 0.0 0.0)::acc) m (n - 1)
      in
      (aux 0 [] edge_index 12)
    in


    let compute_triangles_list l cube =
      let cube_index = compute_cube_index cube in
      let cube_vertices = Box.vertices_list cube in
      let vertices = Array.of_list (compute_vertices_list cube_index cube_vertices) in
      let vlist = Tables.tri_table.(cube_index) in
      let aux2 a b c = Mesh.triangle vertices.(a) vertices.(b) vertices.(c) in
      let rec aux acc = function a::b::c::tl when not (a == -1) -> aux ((aux2 a b c)::acc) tl
                           |_ -> acc
      in
      aux l vlist
    in

    let rec mmc acc i j k =
      let cube = Box.box (0, 0, 0) (i, j , k) in
      if (i == r_x) then mmc (compute_triangles_list acc cube) 0 (j + 1) k
      else if (j == r_y) then mmc (compute_triangles_list acc cube) 0 (j + 1) k
      else if (j == r_y) then mmc (compute_triangles_list acc cube) 0 0 (k + 1)
      else if (k == r_z) then  (List.rev acc)
      else mmc (compute_triangles_list acc cube) (i + 1) j  k
    in
    Mesh.mesh (mmc [] 0 0 0)
;;
end
