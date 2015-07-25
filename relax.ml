(* relax.ml *)

(* Workaround for lack of "for i = start to finish by stride do ... done"
   construct in ocaml.

   ocaml has "for i = start to finish do ... done"

   Too annoying to use a ref with while, or write loop function.
*)
let iter_stride start finish stride f =
  let rec loop i =
    if i > finish then
      ()
    else begin
      f i;
      loop (i+stride)
    end
  in
  loop start


(* Poisson equation.
   \nabla^2 u = f

   Discretized:
   v[i-1][j] + v[i+1][j] + v[i][j-1] + v[i][j+1] - 4 v[i][j] = h^2 f[i][j]

   where the spacing
   h = 1/(n-1)
*)


(* Update v.(i).(j)
   Poisson.
*)
let update size v =
  assert (size = Array.length v);
  let h = 1.0 /. float (size - 1) in
  let h2 = h *. h in
  fun f i j ->
    0.25 *. (v.(i-1).(j) +. v.(i+1).(j) +.
               v.(i).(j-1) +. v.(i).(j+1) -.
               h2 *. f.(i).(j))


(* Residual at a point in the grid.
   Poisson.
*)
let residual_point size v =
  assert (size = Array.length v);
  let h = 1.0 /. float (size - 1) in
  let h2_inv = 1.0 /. (h *. h) in
  fun f i j ->
    f.(i).(j) -.
      h2_inv *. (v.(i-1).(j) +. v.(i+1).(j) +.
                   v.(i).(j-1) +. v.(i).(j+1) -.
                   4.0 *. v.(i).(j))

(*
(* Residual.
   Discretized from r = f - Av.

   Effects:  fill in r.

   Boundaries should be exact.
*)
let residual size r v f =
  let residual_point_v = residual_point size v in
  let size_1 = size - 1 in

  Array.fill r.(0) 0 size 0.0;
  Array.fill r.(size_1) 0 size 0.0;
  for i = 1 to size_1 do
    r.(i).(0) <- 0.0;
    r.(i).(size_1) <- 0.0;

    for j = 1 to size_1 do
      r.(i).(j) <-  residual_point_v f i j
    done
  done
*)
(* Residual.
   Discretized from r = f - Av.

   Boundaries should be exact.
*)
let residual size v f =
  let residual_point_v = residual_point size v in
  let size_1 = size - 1 in
  Array.init size
    (function i ->
      if i = 0 || i = size_1 then
        Array.make size 0.0
      else
        Array.init size
          (function j ->
            if j = 0 || j = size_1 then
              0.0
            else
              residual_point_v f i j))


(* Relative residual.

   r = residual
*)
let rel_residual r f =
  Grid.norm r /. Grid.norm f


(* Suggested measure of accuracy. *)
let is_accurate rel_r =
  rel_r < 1e-6


(* Given an update function and a weight omega, return an
   update function incorporating weighting.
*)
let weighted update omega =
  let omega' = 1.0 -. omega in
  function v ->
    let update_v = update v in
    fun f i j ->
      omega *. update_v f i j +.
        omega' *. v.(i).(j)


(* Relaxation method.
   Gauss-Seidel, red-black, weighted.

   Generic, for 2d.

   Av = f

   Effects:  v is modified.

   Notes: boundaries are untouched.

   Mutation is the whole point of this method, which reuses space and
   saves time as well.
*)
let gs_relax omega size v f =
  assert (size = Array.length v);
  let update = weighted (update size) omega in

  let size_2 = size - 2 in

  (* Red. *)
  for i = 1 to size_2 do
    iter_stride ((i+1) mod 2 + 1) size_2 2
      (function j -> v.(i).(j) <- update v f i j)
  done;
  if !Globals.debug > 2 then begin
    print_endline "After red Gauss-Seidel:";
    Grid.print_state v;
  end;

  (* Black. *)
  for i = 1 to size_2 do
    iter_stride (i mod 2 + 1) size_2 2
      (function j -> v.(i).(j) <- update v f i j)
  done;
  if !Globals.debug > 2 then begin
    print_endline "After black Gauss-Seidel:";
    Grid.print_state v;
  end;
  if !Globals.debug > 1 then begin
    print_endline "After Gauss-Seidel:";
    Grid.print_state v;
  end



(* Relaxation method.
   Jacobi, weighted.

   Generic, for 2d.

   Av = f

   Effects:  v is modified.

   Notes:  boundaries are untouched.

   Purely functional.  This method is highly parallelizable.
   To mutate, have to make copy first.
*)
let jacobi_relax omega size v f =
  assert (size = Array.length v);
  let update = weighted (update size) omega in

  let size_1 = size - 1 in
  let v' =
    Array.init size
      (function i ->
        if i = 0 || i = size_1 then
        (* Don't need to make a copy. *)
          v.(i)
        else
          Array.init size
            (function j ->
              if j = 0 || j = size_1 then
                v.(i).(j)
              else
                update v f i j))
  in
  Array.blit v' 0 v 0 size;
  if !Globals.debug > 1 then begin
    print_endline "After Jacobi";
    Grid.print_state v
  end
