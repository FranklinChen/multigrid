(* grid.ml *)

(* Print current state of grid values.
*)
let print_state v =
  print_endline "begin";
  Array.iter
    (function vi ->
      Array.iter (Printf.printf " %0.6g    ") vi;
      print_newline ())
    v;
  print_endline "end"


(* Generate VRML. *)
let generate_elevation v =
  let dimension = Array.length v in
  let spacing = 1.0 /. float (dimension - 1) in
  print_string "#VRML V2.0 utf8\n\
  \n\
  Shape {\n\
    appearance Appearance {\n\
        material Material { diffuseColor 0 0 1 }\n\
    }
    geometry ElevationGrid {\n\
      height [ ";
  Array.iter (Array.iter (Printf.printf " %0.6g   ")) v;
  Printf.printf " ]\n\
    xDimension %d\n\
    zDimension %d\n\
    xSpacing %0.6g\n\
    zSpacing %0.6g\n\
    }\n\
  }\n" dimension dimension spacing spacing




(* L2 norm of matrix.
*)
let norm =
  Array.fold_left
    (Array.fold_left
       (fun s x -> s +. x *. x))
    0.0

