(* relax.mli *)

(* Residual.
   Discretized from r = f - Av.

   Boundaries should be exact.
*)
val residual : int ->
  float array array -> float array array -> float array array


(* Relative residual.
*)
val rel_residual : float array array -> float array array -> float


val is_accurate : float -> bool

(* Given an update function and a weight omega, return an
   update function incorporating weighting.
*)
val weighted :
  (float array array -> float array array -> int -> int -> float) ->
  float -> float array array -> float array array -> int -> int -> float

(* Relaxation method.
   Gauss-Seidel, red-black, weighted.

   Generic, for 2d.

   Av = f

   Notes: boundaries are untouched.

   Mutation is the whole point of this method, which reuses space and
   saves time as well.  The gain is not reflected here though, because
   I am simulating purely functional.
*)
val gs_relax :
  float -> int ->
    float array array -> float array array -> unit

(* Relaxation method.
   Jacobi, weighted.

   Generic, for 2d.

   Av = f

   Notes:  boundaries are untouched.

   Purely functional.  This method is highly parallelizable.
*)
val jacobi_relax :
  float -> int ->
    float array array -> float array array -> unit
