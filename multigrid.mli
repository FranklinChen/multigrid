(* multigrid.mli *)

val iterate_method :
    (float array array -> float array array -> unit) ->
      float array array -> float array array -> unit

val mu_method :
  (int -> float array array -> float array array -> unit) ->
  int ->
  int -> int -> int -> int -> float array array -> float array array -> unit


(*
   Full multigrid with mu-cycle.

   nu0 = number of mu-cycles for each phase
   nu1 = number of relaxation steps in beginning of each cycle
   nu2 = number of relaxation steps at end of each cycle
   mu = number of recursive calls in each cycle
   coarse_size = size of coarsest grid
*)
val fmu_method :
  (int -> float array array -> float array array -> unit) ->
  int ->
  int ->
  int ->
  int -> int -> int ->
    float array array -> float array array -> unit
