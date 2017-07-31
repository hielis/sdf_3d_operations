open Mesh
open Kernel
open Render
open Parse
open Printf

let sphere_func x y z = (x *. x +. y *. y +. z *. z -. 0.25);;
let sphere_bound = (2.0, 2.0, 2.0);;
let res = (100, 100, 100);;
let h = 1.0;;

let box = Box.box (-. h, -. h, -.h /. 2.0 ) (h, h, h/. 2.0);;
let sphere = Field.field sphere_func box;;

(*let mesh_sphere = SdfRenderMaker.render_a_mesh 0.0 sphere res box;;
 *)
(*let obj = SdfRenderMaker.export_to_obj mesh_sphere;;*)

let grid = parse_sdf "test.sdf";;
let hat_sdf = Field.interpolate_field grid;;
let grid2 = parse_sdf "mathis.sdf";;
let head_sdf = Field.interpolate_field grid2;;
let b1 = Field.boundaries hat_sdf;;
let b2 = Field.boundaries head_sdf;;
let box_fit = Box.max_box b1 b2;;


let test_sdf = FieldOperation.union hat_sdf head_sdf;;
let mesh = SdfRenderMaker.render_a_mesh 0.0 test_sdf res box_fit;;

let oc = open_out "test_main.obj";;
fprintf oc "%s" (SdfRenderMaker.export_to_obj mesh);;
close_out oc;;

