(* options.ml *)

open Arg
open Globals

let rec is_power_2 n =
  n = 1 || (n mod 2 = 0 && is_power_2 (n/2))

let ok_grid_size n =
  n >= 3 && is_power_2 (n-1)

let ok_number n =
  n >= 0

(* Defaults. *)
let nu0 = ref 1
let nu1 = ref 1
let nu2 = ref 1
let mu = ref 1
let coarse_size = ref 3

let omega = ref (2.0 /. 3.0)
let relax = ref Relax.gs_relax

let multigrid = ref "full"


let specs =
  [
  ("-escape",
   Set escape,
   "Escape from algorithm when relative residual small?\n\
     \tDefault = false");
  ("-debug",
   Int (function i ->
     if ok_number i then
       debug := i
     else raise (Bad "-debug must be >= 0")),
   "Debugging level\n\
     \tDefault = " ^ string_of_int !debug ^ "\n\
     \t0 = no debug\n\
     \t1 = output initial and final states\n\
     \t2 = output after every relaxation step\n\
     \t3 = output after each read and black step of Gauss-Seidel\n\
     \t4 = output full multigrid residual and its restriction");
  ("-nu0",
   Int (function i ->
     if ok_number i then
       nu0 := i
     else raise (Bad "-nu0 must be >= 0")),
   "Number of mu-cycles for each phase of full multigrid\n\
     \tDefault = " ^ string_of_int !nu0);
  ("-nu1",
   Int (function i ->
     if ok_number i then
       nu1 := i
     else raise (Bad "-nu1 must be >= 0")),
   "Number of pre-cycle relaxation steps in mu-cycle\n\
     \tDefault = " ^ string_of_int !nu1);
  ("-nu2",
   Int (function i ->
     if ok_number i then
       nu2 := i
     else raise (Bad "-nu2 must be >= 0")),
   "Number of post-cycle relaxation steps in mu-cycle\n\
     \tDefault = " ^ string_of_int !nu2);
  ("-mu",
   Int (function i ->
     if ok_number i then
       mu := i
     else raise (Bad "-mu must be >= 0")),
   "Number of recursive calls in a mu-cycle\n\
     \tDefault = " ^ string_of_int !mu);
  ("-coarse",
   Int (function i ->
     if ok_grid_size i then
       coarse_size := i
     else
       raise (Bad "-coarse must be (2^k+1)")),
   "Size of coarsest grid base case (2^k+1)\n\
     \tDefault = " ^ string_of_int !coarse_size);
  ("-size",
   Int (function i ->
     if ok_grid_size i then
       size := i
     else
       raise (Bad "-size must be (2^k+1)")),
   "Size of finest grid base case (2^k+1)\n\
     \tDefault = " ^ string_of_int !size);
  ("-omega",
   Float (function x ->
     if x <= 0.0 || x > 1.0 then
       raise (Bad "-omega must be in (0, 1]")
     else
       omega := x),
   "Weight for relaxation method (in (0, 1])\n\
     \tDefault = " ^ string_of_float !omega);
  ("-relax",
   String (function "gs" -> relax := Relax.gs_relax
     | "jacobi" -> relax := Relax.jacobi_relax
     | _ -> raise (Bad "-relax must be gs or jacobi")),
   "Relaxation method (gs or jacobi)\n\
     \tDefault = gs\n\
     \tgs = Gauss-Seidel\n\
     \tjacobi = Jacobi");
  ("-multigrid",
   String (function "full" -> multigrid := "full"
     | "mucycle" -> multigrid := "mucycle"
     | "gs" -> multigrid := "gs"
     | "jacobi" -> multigrid := "jacobi"
     | _ -> raise (Bad "-multigrid must be full|mucycle|gs|jacobi")),
   "Overall method\n\
     \tDefault = " ^ !multigrid ^ "\n\
     \tfull = full multigrid\n\
     \tmucycle = mu-cycle\n\
     \tgs = Gauss-Seidel\n\
     \tjacobi = Jacobi\n");
   ("-iterations",
   Int (function i ->
     if ok_number i then
       iterations := i
     else raise (Bad "-iterations must be >= 0")),
   "Number of iterations of relaxation\n\
     \tDefault = " ^ string_of_int !iterations ^ "\n\
     \t(Only applicable to -multigrid gs|jacobi)");
  ]

let anon _ =
  raise (Bad "No anonymous arguments supported")

let usage_message = "Usage:"

let _ = parse specs anon usage_message
