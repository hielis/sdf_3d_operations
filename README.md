# sdf_3d_operations
A project to work on 3d representations of objects and Signed Distance Fields in OCaml


Compile with :

	      ocamlopt Kernel.mli Mesh.mli Tables.mli Render.mli
	      ocamlfind ocamlopt -o smurf -linkpkg -package yojson Kernel.ml Mesh.ml Parse.ml Tables.ml Render.ml smurf_me.ml 

