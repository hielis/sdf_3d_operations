open Mesh
open Kernel
open Render
open Parse
open Printf
open Shapes


let h = 1.0;;
let res = (100, 100, 100);;
let box = Box.box (-. h, -. h, -.h /. 2.0 ) (h, h, h/. 2.0);;
let grid = parse_sdf "hat.sdf";;
let hat_sdf = Field.interpolate_field grid;;
let plane = Shape.plane (0., -. 0.5, 0.6) (0., 0.5, 0.) (Field.boundaries hat_sdf);;
let b1 = Field.boundaries hat_sdf;;
let (xmin, xmax), (ymin, ymax), (zmin, zmax) = b1.Box.x, b1.Box.y, b1.Box.z;;
let grid = parse_sdf "mathis.sdf";;
let head_sdf = Field.interpolate_field grid;;
let cut = FieldOperation.substraction head_sdf plane;;
let hat_scaled = FieldOperation.scale (1., 1., 1.2) hat_sdf;;
let merge = FieldOperation.union cut hat_scaled;;
let scaled =  FieldOperation.scale (0.7, 0.7, 0.7) merge;;


let mesh = SdfRenderMaker.render_a_mesh 0.0 scaled res (Field.boundaries merge);;

let oc = open_out "test_main.obj";;
fprintf oc "%s" (SdfRenderMaker.export_to_obj mesh);;
close_out oc;;

