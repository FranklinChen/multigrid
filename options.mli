(* options.mli *)

val nu0 : int ref
val nu1 : int ref
val nu2 : int ref
val mu : int ref
val coarse_size : int ref

val omega : float ref
val relax : (float -> int ->
  float array array -> float array array -> unit) ref

val multigrid : string ref
