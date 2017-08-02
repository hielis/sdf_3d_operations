open Tables
open Kernel
open Mesh

module SdfRenderMaker : sig
  val export_to_obj : Mesh.mesh -> string
  val render_a_mesh : float -> Field.t -> (int * int * int) -> float Box.box -> Mesh.mesh
end = struct

  let export_to_obj m =
    let vertices = Array.to_list (Mesh.get_vertices_array m) in
    let faces = Mesh.get_faces_list m in

    let face_to_string f =
      let a, b, c = f in
      let lp = List.map string_of_int ([a; b; c]) in
      (String.concat " " ("f "::lp))
    in

    let vertice_to_string v =
      let x, y, z = Vector.get_x v, Vector.get_y v, Vector.get_z v in
      String.concat " " ("v "::(List.map string_of_float [x; y; z]))
    in
    String.concat "\n \n" [String.concat "\n" (List.map vertice_to_string vertices); String.concat "\n" (List.map face_to_string faces)]
  ;;

  let render_a_mesh iso f res box =

    let r_x, r_y, r_z = res in

    let step (a, d) r =
      (d -. a) /. (float_of_int r)
    in

    let h_x, h_y, h_z = step box.Box.x r_x, step box.Box.y r_y, step box.Box.z r_z in
    let (o_x,_), (o_y, _), (o_z, _) = box.Box.x, box.Box.y, box.Box.z in

    let get_array_of_values =
      let a = Array.make (r_x * r_z * r_y) 0.0 in
      let f i j k = (Field.eval f ((float_of_int i) *.h_x +. o_x) ((float_of_int j) *.h_y +. o_y) ((float_of_int k) *.h_z +. o_z)) in
      let rec fill_array i j k = match i, j, k with
        | i, j, k when (i ==  r_x) -> fill_array (0) (j + 1) (k)
        | i, j, k when (j ==  r_y) -> fill_array (0) (0) (k + 1)
        | i, j, k when (k ==  r_z) -> ()
        |i, j, k -> (a.(i + r_x * j + r_x * r_y * k) <- f i j k);
                    fill_array (i + 1) (j) (k);
      in
      fill_array 0 0 0;
      a
    in

    let elements = get_array_of_values in
      let n = Array.length elements in

    let callable_grid (a, b, c) =
      let p = (a + r_x * b + r_y * r_x * c) in
      if (p < n) then (
        elements.(p))
      else failwith "Out of bounds"
    in

    let interpolate p1 p2 v1 v2=
      Vector.add (p1) (Vector.scal_mult ((iso -. v1) /. (v2 -. v1)) (Vector.sub p2 p1))
    in

    let compute_cube_index cube =
      let a = Array.of_list ((Box.vertices_list cube)) in
      let index = ref 0 in
      for i = 0 to 7 do
        if (callable_grid (a.(i)) < iso) then index := (!index) lor (1 lsl i);
      done;
      !index
     in

    let to_vect (cx, cy, cz) = Vector.vect ((float_of_int cx) *.h_x +. o_x) ((float_of_int cy) *.h_y +. o_y) ((float_of_int cz) *.h_z +. o_z) in


    let compute_vertices_list cube_index vertices_list =
      let edge_index = Tables.edge_table.(cube_index) in
      let rec aux i acc m = function (0) -> List.rev acc
                                    |n when (((1 lsl i) land m) != 0)  ->
                                      let e1, e2 = Tables.edge_list.(i) in
                                      let p1, p2 = List.nth vertices_list e1, List.nth vertices_list e2 in
                                      let q1, q2 = to_vect p1, to_vect p2 in
                                      let v1, v2 = callable_grid p1, callable_grid p2 in
                                      aux (i + 1) ((interpolate q1 q2 v1 v2)::acc) m (n -1)
                                    |n -> aux (i + 1) ((Vector.vect 0.0 0.0 0.0)::acc) m (n - 1)
      in
      let l = (aux 0 [] edge_index 12) in
      l
    in

    let compute_triangles_and_vertices triangles cube =
      let index = compute_cube_index cube in
      let l = Box.vertices_list cube in
      let vertices = Array.of_list (compute_vertices_list index l) in
      let table = Tables.tri_table.(index) in
      let rec loop acc = function a::b::c::tl when (a == -1) -> acc
                                 |a::b::c::tl -> 
                                   (*print_string (Mesh.print_triangle ((Mesh.triangle vertices.(a) vertices.(b) vertices.(c))));*)
                                   (try loop ((Mesh.triangle vertices.(a) vertices.(b) vertices.(c))::acc) tl with Invalid_argument s -> failwith "Not enough vertices")
                                 | _ -> acc
      in
      loop triangles table
    in

    let make_list_of_cubes step =
      let rec loop acc i j k =
        if (i > (r_x - step - 2)) then (loop ((Box.box (i, j, k) (i + 1, j + 1, k + 1))::acc) 0 (j + 1) k)
        else if (j >  (r_y - step - 1)) then (loop acc 0 0 (k + step))
        else if (k > (r_z - step - 1)) then (acc)
        else (loop ((Box.box (i, j, k) (i + step, j + step, k + step))::acc) (i + step) j k)
      in
      loop [] 0 0 0
    in
    let f acc c = compute_triangles_and_vertices acc c in
    Mesh.mesh (List.fold_left f [] (List.rev (make_list_of_cubes 1)))
;;


Tables.tri_table;;

end

