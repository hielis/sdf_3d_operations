open Tables
open Kernel
open Mesh


module Pile : sig
   type 'a pile
   exception Empty
   val empty : unit -> 'a pile
   val push : 'a pile -> 'a -> unit
   val pop : 'a pile -> 'a option
   val flush : 'a pile -> 'a list
end = struct
     exception Empty
     type 'a pile = 'a list ref
     let empty () = ref [];;
     let push p a = p:= a::!p;;
     let pop p =
       let a = try (Some (List.hd !p)) with Failure _ -> None in
       p := (try (List.tl !p) with Failure _ -> []);
       a;;
     let flush p =
       let h = !p in
       p:= [];
       h
     ;;
end


module SdfRenderMaker : sig
  val export_to_obj : Mesh.mesh -> string
  val render_a_mesh : float -> Field.t -> (int * int * int) -> float Box.box -> Mesh.mesh
  val render_a_mesh_fast : float -> Field.t -> (int * int * int) -> float Box.box -> Mesh.mesh

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
    String.concat "\n \n" [String.concat "\n" (List.map vertice_to_string vertices); String.concat "\n" (List.rev (List.rev_map face_to_string faces))]
  ;;

  let render_a_mesh_fast iso f res box =
    let r_x, r_y, r_z = res in
    (*let tbl = IntTbl.create (r_x * r_y * r_z) in*)
    
    let edge_pile = Pile.empty () in
    let cube_pile = Pile.empty () in
    let last_cube = ref (Box.box (0, 0, 0) (1, 1, 1)) in
    let step (a, d) r =
      (d -. a) /. (float_of_int r)
    in
    let h_x, h_y, h_z = step box.Box.x r_x, step box.Box.y r_y, step box.Box.z r_z in
    let (o_x,_), (o_y, _), (o_z, _) = box.Box.x, box.Box.y, box.Box.z in
    let array = Array.make (r_x * r_z * r_y) None in
    let is_computed = Array.make (r_x * r_z * r_y) false in
    let callable_grid (a, b, c) =
      let p = (a + r_x * b + r_y * r_x * c) in
      let q = (try array.(p) with _ -> failwith "OutofBounds") in
      match q with
      |Some v -> v
      |None ->
        let value = (Field.eval f ((float_of_int a) *.h_x +. o_x) ((float_of_int b) *.h_y +. o_y) ((float_of_int c) *.h_z +. o_z)) in
        array.(p) <- (Some value);
        value
    in
    let flushed = ref false in
    let count_f = ref 0 in
    let count = ref 0 in
    let update_flushed () =
      if (!flushed) then ()
      else (
        print_string "Flushing the pile";
        flushed := true;
       )
    in
  
  (*refine this*)
    let interpolate p1 p2 v1 v2=
  
        Vector.add (p1) (Vector.scal_mult ((iso -. v1) /. (v2 -. v1)) (Vector.sub p2 p1))
    in
  
  (*end : refine this*)
  
  
    let hash_cube b =
      let (a, _),(b, _), (c, _) = b.Box.x, b.Box.y, b.Box.z in
      (a + r_x * b + r_y * r_x * c)
    in
  
    let compute_cube_index cube =
      let a = Array.of_list ((Box.vertices_list cube)) in
      let index = ref 0 in
      for i = 0 to 7 do
        match (callable_grid (a.(i))) with
        |v when (v < iso) ->  index := ((!index) lor (1 lsl i));
        |_ -> ()
      done;
      !index
    in
  
    let to_vect (cx, cy, cz) = Vector.vect ((float_of_int cx) *.h_x +. o_x) ((float_of_int cy) *.h_y +. o_y) ((float_of_int cz) *.h_z +. o_z) in
  
    let compute_vertices_list cube_index vertices_list =
      let edge_index = Tables.edge_table.(cube_index) in
      let rec aux i acc m =
        function (0) -> List.rev acc
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
  
    let compute_face_index a b c =
     ((1 lsl a) lor (1 lsl b) lor (1 lsl c))
    in
  
    let rec face_index_from_list acc = function [] -> acc | a::tl -> face_index_from_list (acc lor (1 lsl a)) tl in
  
  let add_cubes_to_pile cube =
      let l = List.rev (Pile.flush edge_pile) in
      let xmin, _ = cube.Box.x
      and ymin, _ = cube.Box.y
      and zmin, _ = cube.Box.z in
      let f_i = face_index_from_list 0 l in
      (*
      print_int f_i;
      print_string " ";
      *)
      if (f_i > 0) then (update_flushed ()) else ();
  
      let lp = Tables.face_table.(f_i) in
      let which_cube = function
        |a when a = 1 -> if ((zmin + 1) > (r_z - 2)) then None
                         else (Some ((Box.box (xmin, ymin, zmin + 1) (xmin + 1, ymin + 1, zmin + 2))))
        |a when a = 2 -> if ((zmin - 1) < 0) then None
                         else (Some (Box.box (xmin, ymin, zmin -1) (xmin + 1, ymin + 1, zmin)))
        |a when a = 3 -> if ((ymin + 1) > (r_y - 2)) then None
                         else Some (Box.box (xmin, ymin + 1, zmin) (xmin + 1, ymin + 2, zmin + 1))
        |a when a = 4 -> if ((ymin - 1) < 0) then None
                         else (Some (Box.box (xmin, ymin - 1, zmin) (xmin + 1, ymin, zmin + 1)))
  
        |a when a = 5 -> if ((xmin + 1) > (r_x - 2)) then None
                         else (
                           let b = Box.box (xmin + 1, ymin, zmin) (xmin + 2, ymin + 1, zmin + 1) in
              Some b)
  
        |a when a = 6 -> if ((xmin - 1) < 0) then None
                         else (
                           let b = Box.box (xmin - 1, ymin, zmin) (xmin, ymin + 1, zmin + 1) in
              Some b)
  
        |a when a = 7 -> if (((xmin - 1) < 0) || (ymin + 1 > r_y - 2)) then None
                         else (
                           let b = Box.box (xmin - 1, ymin + 1, zmin) (xmin, ymin + 2, zmin + 1) in
              Some b)
  
        |a when a = 8 -> if (((xmin + 1) > r_x - 2) || (ymin + 1 > r_y - 2)) then None
                         else (
                           let b = Box.box (xmin + 1, ymin + 1, zmin) (xmin + 2, ymin + 2, zmin + 1) in
              Some b)
  
        |a when a = 9 -> if (((xmin + 1) > r_x - 2) || (ymin - 1 > 0)) then None
                         else (
                           let b = Box.box (xmin + 1, ymin - 1, zmin) (xmin + 2, ymin, zmin + 1) in
              Some b)
  
        |a when a = 10 -> if (((xmin - 1) < 0) || (ymin - 1 < 0)) then None
                         else (
                           let b = Box.box (xmin - 1, ymin - 1, zmin) (xmin, ymin, zmin + 1) in
              Some b)
  
        |a when a = 11 -> if (((xmin - 1) < 0 || (ymin + 1 > r_y - 2) || (zmin - 1 < 0))) then None
                         else (
                           let b = Box.box (xmin - 1, ymin + 1, zmin - 1) (xmin, ymin + 2, zmin) in
              Some b)
  
        |a when a = 12 -> if (((xmin - 1) < 0) || (zmin - 1 < 0)) then None
                         else (
                           let b = Box.box (xmin - 1, ymin, zmin - 1) (xmin, ymin + 1, zmin) in
              Some b)
  
        |a when a = 13 -> if (((xmin - 1) < 0) || (zmin - 1 < 0) || (ymin - 1 < 0)) then None
                         else (
                           let b = Box.box (xmin - 1, ymin - 1, zmin - 1) (xmin, ymin, zmin) in
              Some b)
  
        |a when a = 14 -> if ((zmin - 1 < 0) || (ymin - 1 < 0)) then None
                         else (
                           let b = Box.box (xmin, ymin - 1, zmin - 1) (xmin + 1, ymin, zmin) in
              Some b)
  
        |a when a = 15 -> if ((zmin - 1 < 0) || (ymin - 1 < 0)|| (xmin + 1 > r_x - 2)) then None
                         else (
                           let b = Box.box (xmin + 1, ymin - 1, zmin - 1) (xmin + 2, ymin, zmin) in
              Some b)
  
        |a when a = 16 -> if ((zmin - 1 < 0) || (xmin + 1 > r_x - 2)) then None
                         else (
                           let b = Box.box (xmin + 1, ymin, zmin - 1) (xmin + 2, ymin + 1, zmin) in
              Some b)
  
        |a when a = 17 -> if ((zmin - 1 < 0) || (ymin + 1 > r_y - 2)|| (xmin + 1 > r_x - 2)) then None
                         else (
                           let b = Box.box (xmin + 1, ymin + 1, zmin - 1) (xmin + 2, ymin + 2, zmin) in
              Some b)
  
        |a when a = 18 -> if ((zmin - 1 < 0) || (ymin + 1 > r_y - 2)) then None
                         else (
                           let b = Box.box (xmin, ymin + 1, zmin - 1) (xmin + 1, ymin + 2, zmin) in
              Some b)
  
        |a when a = 19 -> if (((xmin - 1) < 0 || (ymin + 1 > r_y - 2) || (zmin + 1 > r_z - 2))) then None
                         else (
                           let b = Box.box (xmin - 1, ymin + 1, zmin + 1) (xmin, ymin + 2, zmin + 2) in
              Some b)
  
        |a when a = 20 -> if (((xmin - 1) < 0) || (zmin + 1 > r_z - 2)) then None
                         else (
                           let b = Box.box (xmin - 1, ymin, zmin + 1) (xmin, ymin + 1, zmin + 2) in
              Some b)
  
        |a when a = 21 -> if (((xmin - 1) < 0) || (zmin + 1 > r_z - 2) || (ymin - 1 < 0)) then None
                         else (
                           let b = Box.box (xmin - 1, ymin - 1, zmin + 1) (xmin, ymin, zmin + 2) in
              Some b)
  
        |a when a = 22 -> if ((zmin + 1 > r_z - 2) || (ymin - 1 < 0)) then None
                         else (
                           let b = Box.box (xmin, ymin - 1, zmin + 1) (xmin + 1, ymin, zmin + 2) in
              Some b)
  
        |a when a = 23 -> if ((zmin + 1 > r_z - 2) || (ymin - 1 < 0)|| (xmin + 1 > r_x - 2)) then None
                         else (
                           let b = Box.box (xmin + 1, ymin - 1, zmin + 1) (xmin + 2, ymin, zmin + 2) in
              Some b)
  
        |a when a = 24 -> if ((zmin + 1 > r_z - 2) || (xmin + 1 > r_x - 2)) then None
                         else (
                           let b = Box.box (xmin + 1, ymin, zmin + 1) (xmin + 2, ymin + 1, zmin + 2) in
              Some b)
  
        |a when a = 25 -> if ((zmin + 1 > r_z - 2) || (ymin + 1 > r_y - 2)|| (xmin + 1 > r_x - 2)) then None
                         else (
                           let b = Box.box (xmin + 1, ymin + 1, zmin + 1) (xmin + 2, ymin + 2, zmin + 2) in
              Some b)
  
        |a when a = 26 -> if ((zmin + 1 > r_z - 2) || (ymin + 1 > r_y - 2)) then None
                         else (
                           let b = Box.box (xmin, ymin + 1, zmin + 1) (xmin + 1, ymin + 2, zmin + 2) in
              Some b)
  
  
  
  
        |_ -> None
      in
      let rec aux2 = function [] -> ()
                      |a::tl ->
                        (match (which_cube a) with
                        |None -> ()
                        |Some c ->
                                   Pile.push cube_pile c;
                                   aux2 tl
                        )
  
      in
      aux2 lp
    in
  
    let compute_triangles_and_vertices triangles cube =
      let index = compute_cube_index cube in
      let l = Box.vertices_list cube in
      let vertices = Array.of_list (compute_vertices_list index l) in
      let table = Tables.tri_table.(index) in
      let rec loop acc = function a::b::c::tl when (a == -1) -> acc
                                 |a::b::c::tl ->
                                   Pile.push edge_pile a;
                                   Pile.push edge_pile b;
                                   Pile.push edge_pile c;
                                   (try
                                      loop ((Mesh.triangle vertices.(a) vertices.(b) vertices.(c))::acc) tl
                                    with Invalid_argument s -> failwith "Not enough vertices")
                                 | _ -> acc
      in
      loop triangles table
    in
  
    let rec launch_computation acc =
      add_cubes_to_pile !last_cube;
      match Pile.pop cube_pile with
      |None -> (print_int !count); print_string "\n";
               acc
      |Some c -> (*print_int (hash_cube c);
                 print_string "\n";*)
        if (is_computed.(hash_cube c)) then (launch_computation acc)
        else(
           is_computed.(hash_cube c) <- true;
           last_cube := c;
           count := !count + 1;
           launch_computation (compute_triangles_and_vertices acc c)
         )
    in

    let init step =
            let j = r_y / 2 in
            for i = 0 to (r_x - 2) do
                for k = 0 to (r_z - 2) do
                   Pile.push cube_pile (Box.box (i, j, k) (i     + step, j + step, k + step));
               done
            done;
    in
    init 1;
    let list_of_triangles = launch_computation [] in
    Mesh.mesh list_of_triangles
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
    Mesh.mesh (List.fold_left f [] ((make_list_of_cubes 1)))
;;





Tables.tri_table;;

end

