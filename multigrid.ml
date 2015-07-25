(* multigrid.ml *)

(* Multigrid on 2d grid.

   Standard trick of overlaying memory, reusing space.

   Note: I earlier implemented this purely functionally, for
   pedagogical reasons, but obviously that is not the way to do this,
   without compiler support.

   I would like to investigate much more elegant implementations in a
   language that supports functional arrays better.
*)


(* Simple-minded iteration of a relaxation.

   Globals:  !Globals.iterations.
*)
let iterate_method m vh fh =
  for i = !Globals.iterations downto 0 do
    m vh fh
  done


(* Linear interpolation on 2d.

   v2h = coarse grid:  (2^k+1) x (2^k+1)
   vh = fine grid:    (2^(k+1)+1) x (2^(k+1)+1)
   size = size of fine grid

   - Inherit points from coarse.
   - Interpolate points in each dimension.

   Example: (n = 2; 3 -> 5)
   x x x          xxxxx
                  xxxxx
   x x x          xxxxx
                  xxxxx
   x x x          xxxxx

   Generally: (n+1 to 2n+1)

   Formula is in Briggs, p. 39.

   Instead of making a single pass, we could make several passes and
   mutate, to avoid branches and reuse intermediate results.
*)
let interpolate size v2h =
  Array.init size
    (function i ->
      let i' = i/2 in
      if i mod 2 = 0 then
        Array.init size
          (function j ->
            let j' = j/2 in
            if j mod 2 = 0 then
              v2h.(i').(j')
            else
              0.5 *. (v2h.(i').(j') +. v2h.(i').(j'+1)))
      else
        Array.init size
          (function j ->
            let j' = j/2 in
            if j mod 2 = 0 then
              0.5 *. (v2h.(i').(j') +. v2h.(i'+1).(j'))
            else
              0.25 *. (v2h.(i').(j') +. v2h.(i'+1).(j') +.
                        v2h.(i').(j'+1) +. v2h.(i'+1).(j'+1))))


(* Restriction.
   Full weighting.

   size = size of fine grid

   Formula is in Briggs, p. 40.

   Preserve boundaries.
*)
let restrict size vh =
  let v2h_length = (size + 1) / 2 in
  let v2h_length_1 = v2h_length - 1 in
  Array.init v2h_length
    (function i ->
      if i = 0 || i = v2h_length_1 then
        (* Hack to maintain length. *)
        Array.sub vh.(i) 0 v2h_length
      else
        let i2 = 2*i in
        Array.init v2h_length
          (function j ->
            if j = 0 || j = v2h_length_1 then
              vh.(i).(j)
            else
              let j2 = 2*j in
              0.0625 *. (vh.(i2-1).(j2-1) +. vh.(i2-1).(j2+1) +.
                           vh.(i2+1).(j2-1) +. vh.(i2+1).(j2+1) +.
                           2.0 *. (vh.(i2).(j2-1) +. vh.(i2).(j2+1) +.
                                     vh.(i2-1).(j2) +. vh.(i2+1).(j2)) +.
                           4.0 *. vh.(i2).(j2))))


(* Effects:  modifies vh.

   size = size of fine grid

   Interpolate error, then add to approximation.
*)
let correct_fine_grid size vh e2h =
  let eh = interpolate size e2h in
  for i = 0 to pred size do
    for j = 0 to pred size do
      vh.(i).(j) <- vh.(i).(j) +. eh.(i).(j)
    done
  done



(* mu-cycle.
   Generalization of V-cycle.

   vh = initial guess
   nu1 = number of relaxation steps in beginning
   nu2 = number of relaxation steps at end
   mu = number of recursive calls
   coarse_size = size of coarsest grid
   size = size of fine grid
 *)
let mu_method relax nu1 nu2 mu coarse_size size =
  let rec mg size vh fh =
    assert (size = Array.length vh);
    for i = 1 to nu1 do
      relax size vh fh
    done;
    if size <= coarse_size then
      relax size vh fh
    else begin
      let rh = Relax.residual size vh fh in
      let f2h = restrict size rh in
      let v2h_length = (size + 1) / 2 in
      let v2h = Array.make_matrix v2h_length v2h_length 0.0 in
      for i = 1 to mu do
        mg v2h_length v2h f2h
      done;
      correct_fine_grid size vh v2h
    end;
    for i = 1 to nu2 do
      relax size vh fh
    done
  in
  mg size


(*
   Full multigrid with mu-cycle.

   nu0 = number of mu-cycles for each phase
   nu1 = number of relaxation steps in beginning of each cycle
   nu2 = number of relaxation steps at end of each cycle
   mu = number of recursive calls in each cycle
   coarse_size = size of coarsest grid
   size = size of fine grid
*)
let fmu_method relax nu0 nu1 nu2 mu coarse_size size =
  let mu_method' = mu_method relax nu1 nu2 mu coarse_size in
  let rec mg size vh fh =
    if size <= coarse_size then
      relax size vh fh
    else begin
      let rh = Relax.residual size vh fh in
      if !Globals.debug > 3 then begin
        Printf.printf "fmu residual rh:\n";
        Grid.print_state rh;
      end;
      let f2h = restrict size rh in
      if !Globals.debug > 3 then begin
        Printf.printf "fmu residual f2h from rh:\n";
        Grid.print_state f2h;
      end;
      let v2h_length = (size + 1) / 2 in
      let v2h = Array.make_matrix v2h_length v2h_length 0.0 in
      mg v2h_length v2h f2h;
      correct_fine_grid size vh v2h
    end;
    for i = 1 to nu0 do
      mu_method' size vh fh
    done
  in
  mg size
