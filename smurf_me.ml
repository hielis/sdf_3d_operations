open Yojson.Basic
open Mesh
open Kernel
open Mesh
open Kernel
open Render
open Parse
open Printf

(* tiger  *)
let print_float_list l = List.iter (printf "\n%f===") l;;
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
let head_sdf = Field.interpolate_field grid_head;;

let grid_hat = parse_sdf hat_path;;
let hat_sdf = Field.interpolate_field grid_hat;;

(* build planes and boxes *)


(* do operations : *)


(*scale and translate the hat*)

let features_x0, features_y0, _ = array_features.(0)
let features_x16, features_y16, features_z16 = array_features.(16)
let features_x20, features_y20, features_z20 = array_features.(20)
let features_x25, features_y25, features_z25 = array_features.(25)
let features_x28, _, features_z28 = array_features.(28)
let features_x29, _, features_z29 = array_features.(29)
let _, features_y31, features_z31 = array_features.(31)
let features_x32, _, _ = array_features.(32)
let features_x9, features_y9, features_z9 = array_features.(9)
(* let hat_scale_x = 1. *. (abs_float (features_x0 -. features_x16)) *)
(* let hat_scale_y = hat_scale_x *)
(* let hat_scale_z = 1. *. hat_scale_x;; *)

let hat_scale_x = 0.7
let hat_scale_y = 0.8
let hat_scale_z = 0.9;;

print_float hat_scale_x;;
print_float hat_scale_y;;
print_float hat_scale_z;;

let test_box_hat = Field.boundaries hat_sdf;;

let (test_xmin, test_xmax), (test_ymin, test_ymax), (test_zmin, test_zmax) = test_box_hat.Box.x, test_box_hat.Box.y, test_box_hat.Box.z;;

let () = print_float_list [test_xmin;test_xmax;test_ymin;test_ymax;test_zmin;test_zmax];;

let box_hat = Field.boundaries (FieldOperation.scale (hat_scale_x, hat_scale_y, hat_scale_z) hat_sdf)

let (xmin, xmax), (ymin, ymax), (zmin, zmax) = box_hat.Box.x, box_hat.Box.y, box_hat.Box.z

let () = print_float_list [xmin;xmax;ymin;ymax;zmin;zmax];;


(*x of tip of the hat align with the nose bone -> point 28 *)
let hat_center_x = (xmax +. xmin) /. 2.
let hat_center_z = (zmax +. zmin) /. 2.
let center_head_x = features_x28
let make_hat_center_x = center_head_x -. hat_center_x ;;

print_string "\n--hat z----\n";;
print_float hat_center_z;;
print_string "\n--hat z----\n";;

print_string "\n----y31----\n";;
print_float features_y31;;

print_string "\n----xmin---\n";;
print_float xmin;;
print_string "\n-----------\n";;
print_float xmax;;
print_string "\n-----------\n";;
print_float ymin;;
print_string "\n-----------\n";;
print_float ymax;;
print_string "\n-----------\n";;
print_float zmin;;
print_string "\n-----------\n";;
print_float zmax;;
print_string "\n-----------\n";;

print_float hat_center_x;;
print_string "-----------\n";;
print_float center_head_x;;
print_string "-----------\n";;
print_float make_hat_center_x;;
print_string "-----------\n";;

(* let hat_translate_x =  make_hat_center_x;; *)
(* let hat_translate_y = (features_y31 -. ymin)/.1.5;; *)
(* let hat_translate_x =  0. *)
(* let hat_translate_y = 0.;; *)
(* let hat_translate_z = 0. ;; *)
(* z of tip of the hat align with the nose bone -> point 28 *)
let hat_translate_x =  make_hat_center_x;;
let hat_translate_y = features_y31 -. ymin -. 0.45 ;;
let hat_translate_z = zmax -. features_z31 -. 0.37;;
print_string "---z31-----\n";;
print_float hat_translate_z;;
print_string "-----------\n";;

print_string "\n
translate _ y : 
";
print_float hat_translate_y;
print_string "\n";;



let hat_modified_sdf = FieldOperation.translate (hat_translate_x, hat_translate_y, hat_translate_z) (FieldOperation.scale (hat_scale_x, hat_scale_y, hat_scale_z) hat_sdf)


let sub_hat_modified_sdf = FieldOperation.translate (hat_translate_x, hat_translate_y, hat_translate_z) (FieldOperation.scale (hat_scale_x, hat_scale_y, hat_scale_z) hat_sdf)

let union_sdf = (FieldOperation.union head_sdf hat_modified_sdf)
(* let union_sdf = (FieldOperation.substraction head_sdf hat_modified_sdf) *)

let my_func x y z = -.0.5 +. x*.x +. (1.2*.y -. 1.)**2. +. z *. z ;;
let bound = Box.box (-.1.5, -.1.5, -.1.5) (1.5, 1.5, 1.5) ;;
let sphere = Field.field my_func bound;;


(* render and export : *)
let res = (200, 200, 300)

let box_smurf =  Box.max_box (Field.boundaries head_sdf) (Field.boundaries hat_modified_sdf)

let (xming, xmaxg), (yming, ymaxg), (zming, zmaxg) = box_smurf.Box.x, box_smurf.Box.y, box_smurf.Box.z;;

(* let (xming, xmaxg), (yming, ymaxg), (zming, zmaxg) = (-. 1.3, 1.3), (-.1. , 2.5), (-. 1.3, 1.3);; *)

print_string "\n";
print_float xming;
print_string "\n";
print_float yming;
print_string "\n";
print_float zming;
print_string "\n";
print_float xmaxg;
print_string "\n";
print_float ymaxg;
print_string "\n";
print_float zmaxg;
print_string "\n";;

let box = Box.box (-.1.5, -.1.5, -.1.5) (1.5, 1.5, 1.5)
let mesh = SdfRenderMaker.render_a_mesh 0.0 union_sdf res box
(* let mesh = SdfRenderMaker.render_a_mesh_fast 0.0 union_sdf res box *)


let oc = open_out pathout;;
fprintf oc "%s" (SdfRenderMaker.export_to_obj mesh);;
close_out oc;;
