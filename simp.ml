(* simp.ml *)

(* Faithful translation from simp.cc provided by PH.
*)

let main () =
  let n =
    if Array.length Sys.argv > 1 then
      int_of_string (Sys.argv.(1))
    else
      1024
  in let _ =
    Printf.printf "creating %dx%d array\n" (n+1) (n+1)
  in let _, t =
    Cpu.time (function () -> Array.make_matrix (n+1) (n+1) 0.0) ()
  in Printf.printf "took %g seconds\n" t

let _ =
  main ()

