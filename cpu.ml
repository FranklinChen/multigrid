(* cpu.ml *)

open Unix

let clock_cpu_read () =
  let { tms_utime = u;
        tms_stime = s;
        tms_cutime = _;
        tms_cstime = _
      } = times () in
  (u +. s)

let time f arg =
  let t0 = clock_cpu_read () in
  let result = f arg in
  let t1 = clock_cpu_read () in
  result, t1 -. t0

