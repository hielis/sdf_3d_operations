(*
open Kernel
open Mesh
open Render
open Printf
*)

let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
         try Some (input_line channel) with End_of_file -> None);;

let parse_sdf namefile =
  let in_channel = open_in namefile in
  let lines = line_stream_of_channel in_channel in
  let l_grid = try (String.split_on_char ' ' (Stream.next lines)) with Stream.Failure -> failwith "Aie" in
  let l_grid_step = try (String.split_on_char ' ' (Stream.next lines)) with Stream.Failure -> failwith "Aie" in
  let rec aux acc = let a = (try Some(float_of_string (Stream.next lines)) with Stream.Failure -> None) in
                    match a with
                      Some a -> aux (a::acc) | None -> List.rev acc
  in
  let rec aux2 acc = function a::tl -> aux2 ((int_of_string a)::acc) tl | [] -> List.rev acc in
  let rec aux3 acc = function a::tl -> aux3 ((float_of_string a)::acc) tl | [] -> List.rev acc in
  let h = float_of_string (Stream.next lines) in
  let matrix = Array.of_list (aux []) in
  let a::b::c::[] = aux2 [] l_grid in
  let d::e::g::[] = aux3 [] l_grid_step in
  (matrix, (a, b, c), (d, e, g), h);;

