open Yojson.Basic
open Mesh
open Kernel
open Mesh
open Kernel
open Render
open Parse
open Printf

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

(* transform to -1 to 1 box *)
let box = Field.boundaries head_sdf
let (xmin, xmax), (ymin, ymax), (zmin, zmax) = box.Box.x, box.Box.y, box.Box.z;;
let head_trans_x = -.(xmax +. xmin)/.2.;;
let head_trans_y = -.(ymax +. ymin)/.2.;;
let head_trans_z = -.(zmax +. zmin)/.2.;;
let head_scale = 2.0 /. (s *. float_of_int (max gz (max gx gy)));;

let head_sdf = FieldOperation.translate (head_trans_x, head_trans_y, head_trans_z) (Field.interpolate_field grid_head);;

let head_sdf = FieldOperation.scale (head_scale, head_scale, head_scale) head_sdf;;

let () = print_bbox head_sdf;;

(* read from facial features *)
let transform_features_coor f trans_x trans_y trans_z scale=
    let x, y, z = f in
    let l = Array.of_list (List.map (fun x -> x*.scale) [x+.trans_x;y+.trans_y;z+.trans_z]) in
    (l.(0), l.(1), l.(2));;

let head_trans_f f = transform_features_coor f head_trans_x head_trans_y head_trans_z head_scale;;

(* left of left eye *)
let features_x36, features_y36, features_z36 = head_trans_f array_features.(36);;
(* right of right eye *)
let features_x45, features_y45, features_z45 = head_trans_f array_features.(45);;
(* tip of a nose *)
let features_x33, features_y33, features_z33 = head_trans_f array_features.(33);;

let head_length = 2.*.(features_x45 -. features_x36);;
print_string "\nhead head_length?\n";;
print_float head_length;;

(* parse hat *)
let grid_hat = parse_sdf hat_path;;
let _, (hat_gx, hat_gy, hat_gz), (hat_sx, hat_sy, hat_sz), hat_s = grid_hat
let lambda_hat = 2.0 /. (hat_s *. (float_of_int (max hat_gx (max hat_gy hat_gz))));;
let hat_sdf = Field.interpolate_field grid_hat;;

(* transform to -1 to 1 box *)
let bbox =  Field.boundaries hat_sdf;;
let (xmin, xmax), (ymin, ymax), (zmin, zmax) = box.Box.x, box.Box.y, box.Box.z;;
let hat_sdf = FieldOperation.translate (-.(xmax +. xmin)/.2., -.(ymax +. ymin)/.2., -.(zmax +.zmin)/.2.) hat_sdf;;
let hat_sdf = FieldOperation.scale (head_length*.lambda_hat, head_length*.lambda_hat, head_length*.lambda_hat) (hat_sdf);;


let hat_translate_x = features_x33;;
let hat_translate_y =  head_length*.(-1.0) +. features_y33;;
let hat_translate_z = features_z33 +. head_length*.1. -. 0.1;;

let hat_translated_sdf = FieldOperation.translate (hat_translate_x, hat_translate_y, hat_translate_z)  hat_sdf;;

let union_sdf = (FieldOperation.union head_sdf hat_translated_sdf)

(* render and export : *)
let res = (200, 200, 200)

let union_feature f_x f_y f_z sdf =
    let f_func x y z = -.0.001 +. (x -. f_x)**2. +. (y -. f_y)**2. +. (z -. f_z)**2. in
    let bound = Box.box (-.1.5, -.1.5, -.1.5) (1.5, 1.5, 1.5) in
    let sphere = Field.field f_func bound in
    FieldOperation.union sdf sphere;;

(* tip of the nose *)
let union_sdf = union_feature features_x33 features_y33 features_z33 union_sdf;;
let union_sdf = union_feature features_x36 features_y36 features_z36 union_sdf;;
let union_sdf = union_feature features_x45 features_y45 features_z45 union_sdf;;

let box = Box.box (-1.3, -1.3 +. hat_translate_y, -.1.3) (1.3, 1.0, 1.3 +. hat_translate_z)
let mesh = SdfRenderMaker.render_a_mesh_fast 0.0 union_sdf res box;;

let oc = open_out pathout;;
fprintf oc "%s" (SdfRenderMaker.export_to_obj mesh);;
close_out oc;;
