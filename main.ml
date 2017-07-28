open Mesh
open Kernel
open Render

let sphere_func x y z = (x *. x +. y *. y +. z *. z -. 1.0);;
let sphere_bound = (2.0, 2.0, 2.0);;

let sphere = Field.field sphere_func sphere_bound;;
let res = (100, 100, 100);;
let box = Box.box (-1.0, -1.0,-1.0) ( 1.0, 1.0, 1.0);;
let mesh_sphere = SdfRenderMaker.render_a_mesh 0.0 sphere res box;;
