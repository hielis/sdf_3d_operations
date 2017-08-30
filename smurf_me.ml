open Yojson.Basic
open Mesh
open Kernel
open Mesh
open Kernel
open Render
open Parse
open Printf

(*er  *)
let print_float_list l ?h:(headstr = "tiger") =
    print_string "\n";
    print_string headstr;
    print_string "\n";
    List.iter (printf "\n%f===") l;;

let print_bbox sdf =
    let box = Field.boundaries sdf in
    let (xmin, xmax), (ymin, ymax), (zmin, zmax) = box.Box.x, box.Box.y, box.Box.z in
    print_float_list [xmin;xmax;ymin;ymax;zmin;zmax] ~h:"---bbox---";;
let max a b = if a > b then a else b;;

(* end tiger  *)

(* filepaths  *)



let json_string = Sys.argv.(1)
let head_path = Sys.argv.(2)
let hat_path = Sys.argv.(3)
let pathout = Sys.argv.(4)


(* parse json *)

let json = Yojson.Basic.from_string json_string
let to_vect_array () =
  let assoc_to_vect arg  =
    let x = arg |> Yojson.Basic.Util.member "x" |> Yojson.Basic.Util.to_float
    and y = arg	|> Yojson.Basic.Util.member "y" |> Yojson.Basic.Util.to_float
    and z = arg |> Yojson.Basic.Util.member "z" |> Yojson.Basic.Util.to_float in
    (x, y, z)
  in
  let json_list = Yojson.Basic.Util.to_list json in
  let vect_list = List.map assoc_to_vect (json_list) in
  Array.of_list (vect_list);;

let array_features = to_vect_array ();;

(* load sdfs : *)

let grid_head  = parse_sdf head_path;;
let _, (gx, gy, gz), (sx, sy, sz), s = grid_head
let head_sdf = Field.interpolate_field grid_head;;

(* tiger *)
let box = Field.boundaries head_sdf
let (xmin, xmax), (ymin, ymax), (zmin, zmax) = box.Box.x, box.Box.y, box.Box.z;;
let head_trans_x = -.(xmax +. xmin)/.2.;;
let head_trans_y = -.(ymax +. ymin)/.2.;;
let head_trans_z = -.(zmax +. zmin)/.2.;;
let head_scale = 2.0 /. (s *. float_of_int (max gz (max gx gy)));;
(* let head_scale = 8.0 /. (s *. (float_of_int (max gz (max gx gy)))*.3.);; *)
(* let head_scale = 8.0 /. (s *. (float_of_int (gz + gx + gy)));; *)

let head_sdf = FieldOperation.translate (head_trans_x, head_trans_y, head_trans_z) (Field.interpolate_field grid_head);;

let head_sdf = FieldOperation.scale (head_scale, head_scale, head_scale) head_sdf;;

let () = print_bbox head_sdf;;

let grid_hat = parse_sdf hat_path;;
let _, (hat_gx, hat_gy, hat_gz), (hat_sx, hat_sy, hat_sz), hat_s = grid_hat
let lambda_hat = 2.0 /. (hat_s *. (float_of_int (max hat_gx (max hat_gy hat_gz))));;
(* let lambda_hat = 8.0 /. (hat_s *. (float_of_int (max hat_gx (max hat_gy hat_gz)))*.3.);; *)
(* let lambda_hat = 8.0 /. (hat_s *. (float_of_int (hat_gx + hat_gy + hat_gz)));; *)
let hat_sdf = Field.interpolate_field grid_hat;;



(* build planes and boxes *)


(* do operations : *)


(*scale and translate the hat*)
let transform_features_coor f trans_x trans_y trans_z scale=
    let x, y, z = f in
    let l = Array.of_list (List.map (fun x -> x*.scale) [x+.trans_x;y+.trans_y;z+.trans_z]) in
    (l.(0), l.(1), l.(2));;

let head_trans_f f = transform_features_coor f head_trans_x head_trans_y head_trans_z head_scale;;

let features_x0, features_y0, features_z0 = head_trans_f array_features.(0);;
let features_x16, features_y16, features_z16 = head_trans_f array_features.(16)
let features_x20, features_y20, features_z20 = head_trans_f array_features.(20)
let features_x25, features_y25, features_z25 = head_trans_f array_features.(25)
let features_x28, _, features_z28 = head_trans_f array_features.(28)
let features_x29, _, features_z29 = head_trans_f array_features.(29)
let features_x37, features_y37, features_z37 = head_trans_f array_features.(37);;
let features_x46, features_y46, features_z46 = head_trans_f array_features.(46);;
let features_x34, features_y34, features_z34 = head_trans_f array_features.(34);;
let features_x9, features_y9, features_z9 = head_trans_f array_features.(9);;

print_float_list [features_x34; features_y34; features_z34] ~h:"features_x \n";;
(* print_float_list [features_x46; features_y46; features_z46] ~h:"features_x \n";; *)
(* print_float_list [features_x37; features_y37; features_z37] ~h:"features_x \n";; *)

let head_length = 2.*.(features_x46 -. features_x37);;
print_string "\nhead head_length?\n";;
print_float head_length;;

(* let hat_scale_x = 1. *. (abs_float (features_x0 -. features_x16)) *)
(* let hat_scale_y = hat_scale_x *)
(* let hat_scale_z = 1. *. hat_scale_x;; *)

(* let hat_scale_x = lambda *. 0.7 *)
(* let hat_scale_y = lambda *. 0.8 *)
(* let hat_scale_z = lambda *. 0.9;; *)

let () = print_bbox hat_sdf;;

let bbox =  Field.boundaries hat_sdf;;
let (xmin, xmax), (ymin, ymax), (zmin, zmax) = box.Box.x, box.Box.y, box.Box.z;;
let hat_sdf = FieldOperation.translate (-.(xmax +. xmin)/.2., -.(ymax +. ymin)/.2., -.(zmax +.zmin)/.2.) hat_sdf;;

let () = print_bbox hat_sdf;;

let hat_sdf = FieldOperation.scale (head_length*.lambda_hat, head_length*.lambda_hat, head_length*.lambda_hat) (hat_sdf);;

let () = print_bbox hat_sdf;;

(* let box_hat = Field.boundaries (FieldOperation.scale (hat_scale_x, hat_scale_y, hat_scale_z) hat_sdf) *)

(* let (xmin, xmax), (ymin, ymax), (zmin, zmax) = box_hat.Box.x, box_hat.Box.y, box_hat.Box.z *)

(* let () = print_float_list [xmin;xmax;ymin;ymax;zmin;zmax];; *)


(*x of tip of the hat align with the nose bone -> point 28 *)
let hat_center_x = 0.;;
let hat_center_z = (zmax +. zmin) /. 2.;;
let center_head_x = features_x28;;
let make_hat_center_x = center_head_x -. hat_center_x ;;


let hat_translate_x = features_x34;;
let hat_translate_y =  head_length*.(-1.0) +. features_y34;;
let hat_translate_z = features_z34 +. head_length*.1. -. 0.1;;
print_float_list [hat_translate_x;hat_translate_y;hat_translate_z] ~h:"hat translate\n";;


let hat_modified_sdf = FieldOperation.translate (hat_translate_x, hat_translate_y, hat_translate_z)  hat_sdf;;

(* let sub_hat_modified_sdf = FieldOperation.translate (hat_translate_x, hat_translate_y, hat_translate_z) (FieldOperation.scale (hat_scale_x, hat_scale_y, hat_scale_z) hat_sdf) *)

let union_sdf = (FieldOperation.union head_sdf hat_modified_sdf)
(* let union_sdf = (FieldOperation.union hat_sdf head_sdf) *)

(* render and export : *)
let res = (200, 200, 200)

(* let box_smurf =  Box.max_box (Field.boundaries head_sdf) (Field.boundaries hat_modified_sdf) *)

(* let (xming, xmaxg), (yming, ymaxg), (zming, zmaxg) = box_smurf.Box.x, box_smurf.Box.y, box_smurf.Box.z;; *)

(* let (xming, xmaxg), (yming, ymaxg), (zming, zmaxg) = (-. 1.3, 1.3), (-.1. , 2.5), (-. 1.3, 1.3);; *)

let box = Box.box (-1.3, -1.0 +. hat_translate_y, -.1.3) (1.3, 1.3, 1.3)
(* let mesh = SdfRenderMaker.render_a_mesh 0.0 union_sdf res box;; *)
(* let mesh = SdfRenderMaker.render_a_mesh_fast 0.0 union_sdf res box;; *)
let mesh = SdfRenderMaker.render_a_mesh_fast 0.0 union_sdf res box;;

let oc = open_out pathout;;
fprintf oc "%s" (SdfRenderMaker.export_to_obj mesh);;
close_out oc;;
