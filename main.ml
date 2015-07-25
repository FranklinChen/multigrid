(* main.ml *)

open Globals
open Options

let size = !size

(*** Set up input. ***)
let pi = 3.1415926535

(* Actual coordinate based on index into grid.
   Assumes we are on unit square [0,0] x [0,0].
*)
let pos j size =
  float j /. float size

(* Initial guess; note boundary specified.
   FMC Should allow user input.
*)
let v = Array.init size
    (function i ->
      if i = 0 || i = size - 1 then
        Array.make size 0.0
      else
        Array.init size
          (function j ->
            if j = 0 then
              0.0
            else if j = size - 1 then
              sin (pi *. pos j size)
            else
              (* Random initial junk. *)
              0.0))

(* Poisson -> use Laplace equation. *)
let f = Array.make_matrix size size 0.0

(* FMC Use something other than Laplace.
   Should allow input
*)
let f = Array.make_matrix size size 1.0

let _ =
  if !Globals.debug > 0 then begin
    print_endline "Initial guess v";
    Grid.print_state v
  end

(* These will get mutated. *)
let v_final = Array.map Array.copy v
let f_final = Array.map Array.copy f

(* Return latest value, based on global changing v. *)
let fine_rel_residual () =
  Relax.rel_residual (Relax.residual size v_final f_final) f_final



(* Quit early from an algorithm with a relative residual. *)
exception Relax_escape of float

(* Wrapper to trap an early escape.
*)
let escaped_relax =
  function
      false ->
        fun relax s vh fh ->
          relax s vh fh
    | true ->
        fun relax s vh fh ->
          relax s vh fh;
          let rel_r = fine_rel_residual () in
          if Relax.is_accurate rel_r then
            raise (Relax_escape rel_r)

let relax_method = escaped_relax !escape (!relax !omega)

(* Complete algorithm. *)
let multigrid_method =
  function
      "full" -> Multigrid.fmu_method
          relax_method !nu0 !nu1 !nu2 !mu !coarse_size size
    | "mucycle" -> Multigrid.mu_method
          relax_method !nu1 !nu2 !mu !coarse_size size
    | "gs" -> Multigrid.iterate_method
          (escaped_relax !escape (Relax.gs_relax !omega)
             size)
    | "jacobi" -> Multigrid.iterate_method
          (escaped_relax !escape (Relax.jacobi_relax !omega)
             size)


(* Return final relative relaxation. *)
let alg =
  let m = multigrid_method !multigrid in
  fun vh fh ->
    try
      m vh fh;
      fine_rel_residual ()
    with
      Relax_escape rel_r -> rel_r




(* Mutate v_final and f_final. *)
let rel_r, t = Cpu.time (alg v_final) f_final

(* Rudimentary output. *)
let _ =
  Printf.printf "Time = %0.6g\n" t;
  Printf.printf "Relative residual = %0.6g\n" rel_r;
  print_endline ("Good = " ^ if Relax.is_accurate rel_r then "yes" else "no")

let _ =
  if !Globals.debug > 0 then begin
    print_endline "Final result v";
    Grid.print_state v_final;

    Grid.generate_elevation v_final
  end
