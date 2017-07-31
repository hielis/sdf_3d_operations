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


module Vector : sig
  type vect = float * float * float
  type t = vect * int
  val t : vect -> int -> t
  val vect : float -> float -> float -> vect
  val get_x : vect -> float
  val get_y : vect -> float
  val get_z : vect -> float
  val get_i : t -> int
  val scal_mult : float -> vect -> vect
  val dot_product : vect -> vect -> float
  val norm : vect -> float
  val wedge_product : vect -> vect -> vect
  val add : vect -> vect -> vect
  val sub : vect -> vect -> vect
  val equals : vect -> vect -> bool
  val compare : t -> t -> int
end = struct
  type vect = float * float * float
  type t = vect * int
  let vect x y z = (x, y, z);;
  let t v i = (v, i);;
  let get_x = function (x, y, z) -> x;;
  let get_y = function (x, y, z) -> y;;
  let get_z = function (x, y, z) -> z;;
  let get_i = function (_, i) -> i;;
  let scal_mult sc = function (x, y, z) -> (sc *. x, sc *. y, sc *. z);;
  let dot_product v1 v2 =
    let x, y, z = v1 and xp, yp, zp = v2 in
    (x *. xp +. y *. yp +. z *. zp);;
  let norm x = sqrt (dot_product x x);;
  let wedge_product v1 v2 =
    let x, y, z = v1 and xp, yp, zp = v2 in
    (y *. zp -. yp *.z, x *. zp -. xp *.z, x *. yp -. y *.xp);;
  let add v1 v2 =
    let x, y, z = v1 and xp, yp, zp = v2 in
    (x +. xp, y +. yp, z +. zp);;
  let sub v1 v2 = add v1 (scal_mult (-.1.0) v2);;
  let equals v1 v2 =
    let x, y, z = sub v1 v2 in
    ((x == 0.0) && (y == 0.0) && (z == 0.0));;
  let compare (v1p : t) (v2p : t) =
    let ((xq, yq, zq), i) , ((xp, yp, zp), j) = v1p, v2p in
    let x = (xq -. xp) and y = (yq -. yp) and z =  (zq -. zp) in
    match x, y, z with
    |0.0, 0.0, 0.0 -> 0
    |0.0, 0.0, a -> int_of_float (a /. (abs_float a))
    |0.0, a, _ -> int_of_float (a /. (abs_float a))
    |a, _, _ -> int_of_float (a /. (abs_float a))
  ;;
end


(*
module type MeshType =
  functor  (V : VectorType) -> sig
    type triangle
    type face
    type mesh
    exception Invalid_triangle of triangle
    val mesh : triangle list -> mesh
    val face : V.vect -> V.vect -> V.vect -> face
    val triangle : V.vect -> V.vect -> V.vect -> triangle
    val face_to_triangle : mesh -> face -> triangle
    val mesh : triangle list -> mesh
    val get_faces_list : mesh -> face list
    val get_vertices_of_triangle : triangle -> V.vect * V.vect * V.vect
    val get_vectices_of_face : mesh -> face -> V.vect * V.vect * V.vect
    val get_triangles_list : mesh -> triangle list
    val get_vertices_array : mesh -> V.vect array
    val print_triangle : triangle -> string
  end

*)
module type MeshType =
  functor (V : VectorType) ->
    sig
      type triangle = V.vect * V.vect * V.vect
      type face = int * int * int
      type mesh = V.vect array * face list
      exception Invalid_triangle
      val face_to_triangle : 'a array * 'b -> int * int * int -> 'a * 'a * 'a
      val triangle : 'a -> 'b -> 'c -> 'a * 'b * 'c
      val get_vertices_of_triangle : 'a * 'b * 'c -> 'a * 'b * 'c
  (*    val get_vertices_of_face :
        'a array * 'b -> int * int * int -> 'a * 'a * 'a
   *)    
      val get_triangles_list :
        'a array * (int * int * int) list -> ('a * 'a * 'a) list
      val mesh : (V.vect * V.vect * V.vect) list -> V.vect array * face list
      val get_vertices_array : 'a * 'b -> 'a
      val get_faces_list : 'a * 'b -> 'b
      val print_triangle : V.vect * V.vect * V.vect -> string
    end



module MeshMaker : MeshType = functor (V : VectorType) -> struct


                      (*Inspired by the implementation of Set from Ocaml stdlib / my programming class at EP *)
                     module S : sig
                       type s
                       exception Not_found
                       exception Error_Set
                       val add : V.t -> s -> s
                       val empty : s
                       val find : V.t -> s -> V.t
                     end = struct
                       exception Not_found
                       exception Error_Set

                       type s = Empty | Node of {l: s ; v: V.t; r: s; h:int}
                       let create l v r =
                         let hl = match l with Empty -> 0 | Node {h} -> h in
                         let hr = match r with Empty -> 0 | Node {h} -> h in
                         Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)};;

                       let empty = Empty;;

                       let height = function
                           Empty -> 0
                         | Node {h} -> h;;

                       let bal l v r =
                         let hl = match l with Empty -> 0 | Node {h} -> h in
                         let hr = match r with Empty -> 0 | Node {h} -> h in
                         if hl > hr + 2 then begin
                             match l with
                               Empty -> raise Error_Set
                             | Node{l=ll; v=lv; r=lr} ->
                                if height ll >= height lr then
                                  create ll lv (create lr v r)
                                else begin
                                    match lr with
                                      Empty -> raise Error_Set
                                    | Node{l=lrl; v=lrv; r=lrr}->
                                       create (create ll lv lrl) lrv (create lrr v r)
                                  end
                           end else if hr > hl + 2 then begin
                             match r with
                               Empty -> raise Error_Set
                             | Node{l=rl; v=rv; r=rr} ->
                                if height rr >= height rl then
                                  create (create l v rl) rv rr
                                else begin
                                    match rl with
                                      Empty -> raise Error_Set
                                    | Node{l=rll; v=rlv; r=rlr} ->
                                       create (create l v rll) rlv (create rlr rv rr)
                                  end
                           end else
                           Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)};;

                       let rec add x = function Empty -> Node{l=Empty; v=x; r=Empty; h=1}
                                              | Node{l; v; r} as t ->
                                                 let c = V.compare x v in
                                                 if c = 0 then t else
                                                   if c < 0 then
                                                     let ll = add x l in
                                                     if l == ll then t else bal ll v r
                                                   else
                                                     let rr = add x r in
                                                     if r == rr then t else bal l v rr
                       ;;
                       let rec find x = function
                           Empty -> raise Not_found
                         | Node{l; v; r} ->
                            let c = V.compare x v in
                            if c = 0 then v
                            else find x (if c < 0 then l else r);;
                     end

                     type triangle = V.vect * V.vect * V.vect
                     type face = (int * int * int)
                     type mesh = (V.vect array) * (face list)
                     exception Invalid_triangle
                     let face_to_triangle m = function (a, b, c) ->
                                                       let array, list = m in
                                                       ((array.(a)), (array.(b)), (array.(c))) ;;
                     let triangle v1 v2 v3 =
                       (v1, v2, v3);;
                     let get_vertices_of_triangle = function (b, c, d) -> (b, c, d);;
                     let get_vertices_of_face m f = get_vertices_of_triangle (face_to_triangle m f);;
                     let get_triangles_list m =
                       let rec aux acc = function [] -> acc | a::tl -> aux ((face_to_triangle m a)::acc) tl in
                       let a, l = m in
                       aux [] l;;
                        let mesh t_list =
                          let aux1 (l2 : V.vect list) s i a =
                            try
                              let h  = S.find (V.t a 0) s in
                              let _, j = h in
                              (s, j,  l2)
                            with S.Not_found -> (S.add (V.t a (i + 1)) s, i + 1, a::l2)
                          in
                          let max a b = if (a > b) then a else b in

                          let rec aux2 (l1 :face list) (l2 : V.vect list) i s =
                            function [] -> (List.rev l1, List.rev l2)
                                    |a::tl -> let v1, v2, v3  = get_vertices_of_triangle a in
                                              let (s1, i1, l21) = aux1 l2 s i v1 in
                                              let (s2, i2, l22) = aux1 l21 s1 (max i i1) v2 in
                                              let (s3, i3, l23) = aux1 l22 s2 (max i (max i1 i2)) v3 in
                                              aux2 ((i1, i2, i3)::l1) (l23) (max i3 (max i2 (max i i1))) s3 tl
                          in

                          let l_faces, l_vertices = aux2 [] [] 0 (S.empty) t_list in
                          (Array.of_list l_vertices, l_faces);;
                        let get_vertices_array = function (a, l) -> a;;
                        let get_faces_list = function (a, l) -> l;;
                        let print_triangle = function (a, b, c) ->
                                                      let xs = List.map V.get_x ([a; b; c])
                                                      and ys = List.map V.get_y ([a; b; c])
                                                      and zs = List.map V.get_z ([a; b; c]) in
                                                      let aux = function hd1::tl1, hd2::tl2, hd3::tl3 ->
                                                                           String.concat " " ["["; string_of_float hd1; "; "; string_of_float hd2; "; " ; string_of_float hd3; "]"]
                                                                         | _, _, _ -> failwith "Empty list"
                                                      in
                                                      String.concat " || " [aux (xs, ys, zs); aux (List.tl xs, List.tl ys, List.tl zs); aux (List.tl (List.tl xs), List.tl (List.tl ys), List.tl (List.tl zs))]
                        ;;
end

module Mesh = MeshMaker(Vector)
