open Yojson.Basic
open Mesh
open Kernel
open Mesh
open Kernel
open Render
open Parse
open Printf

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
  Array.of_list (List.rev vect_list);;

let array_features = to_vect_array ();;

(* load sdfs : *)

let grid_head  = parse_sdf head_path;;
let head_sdf = Field.interpolate_field grid_head;;

let grid_hat = parse_sdf hat_path;;
let hat_sdf = Field.interpolate_field grid_hat;;

(* build planes and boxes *)


(* do operations : *)


(*scale and translate the hat*)

let _, features_y0, _ = array_features.(0)
let features_x20, features_y20, features_z20 = array_features.(20)
let features_x25, features_y25, features_z25 = array_features.(25)
let features_x28, _, features_z28 = array_features.(28)
let features_x9, features_y9, features_z9 = array_features.(9)
let hat_scale_x = 1.7 *. (abs_float (features_x20 -. features_x25))
let hat_scale_y = hat_scale_x
let hat_scale_z = 1.4 *. hat_scale_x;;
print_float hat_scale_x;;

let box_hat = Field.boundaries (FieldOperation.scale (hat_scale_x, hat_scale_y, hat_scale_z) hat_sdf)
let (xmin, xmax), (ymin, ymax), (zmin, zmax) = box_hat.Box.x, box_hat.Box.y, box_hat.Box.z

let hat_translate_x =  0.
let hat_translate_y = -. 2.3 *. features_y9
let hat_translate_z = 0.



let hat_modified_sdf = FieldOperation.translate (hat_translate_x, hat_translate_y, hat_translate_z) (FieldOperation.scale (hat_scale_x, hat_scale_y, hat_scale_z) hat_sdf)


let union_sdf = FieldOperation.union head_sdf hat_modified_sdf

(* render and export : *)
let res = (200, 200, 200)

let box_smurf = Field.boundaries union_sdf
let (xming, xmaxg), (yming, ymaxg), (zming, zmaxg) = box_smurf.Box.x, box_smurf.Box.y, box_smurf.Box.z;;

print_float xming;
print_float yming;
print_float zming;
print_float xmaxg;
print_float ymaxg;
print_float zmaxg;;

let box = Box.box (1.1 *. xming , 1.1 *. yming, 1.1 *. zming) (1.1 *. xmaxg, 1.7 *. ymaxg, 1.1 *. zmaxg)
let mesh = SdfRenderMaker.render_a_mesh_fast 0.0 union_sdf res box


let oc = open_out pathout;;
fprintf oc "%s" (SdfRenderMaker.export_to_obj mesh);;
close_out oc;;
