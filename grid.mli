(* grid.mli *)

(* Print current state of grid values.
*)
val print_state : float array array -> unit

val generate_elevation : float array array -> unit

(* L2 norm of matrix.
*)
val norm : float array array -> float
