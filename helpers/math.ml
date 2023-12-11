(* https://rosettacode.org/wiki/Evaluate_binomial_coefficients#OCaml *)
let binomialCoeff n p =
  let p = if p < n - p then p else n - p in
  let rec cm res num denum =
    if denum <= p then cm ((res * num) / denum) (num - 1) (denum + 1)
    else res in
  cm 1 n 1
