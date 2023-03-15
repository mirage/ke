let cut ppf _ = Format.pp_print_cut ppf ()
let sp ppf _ = Format.pp_print_space ppf ()
let any fmt ppf _ = Format.fprintf ppf fmt

let box ?(indent = 0) pp_v ppf v =
  Format.(
    pp_open_box ppf indent;
    pp_v ppf v;
    pp_close_box ppf ())

let iter ?sep:(pp_sep = cut) iter pp_elt ppf v =
  let is_first = ref true in
  let pp_elt v =
    if !is_first then (
      is_first := false;
      pp_sep ppf ());
    pp_elt ppf v
  in
  iter pp_elt v

let surround s1 s2 pp_v ppf v =
  Format.(
    pp_print_string ppf s1;
    pp_v ppf v;
    pp_print_string ppf s2)

let parens pp_v = box ~indent:1 (surround "(" ")" pp_v)

let ( ++ ) pp_v0 pp_v1 ppf v =
  pp_v0 ppf v;
  pp_v1 ppf v

let iter_dump iter' pp_name pp_elt =
  let pp_v = iter ~sep:sp iter' (box pp_elt) in
  parens (pp_name ++ sp ++ pp_v)
