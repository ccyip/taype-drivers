type t = Public | Trusted | Private of int

let to_string = function
  | Public -> "public"
  | Trusted -> "trusted"
  | Private x -> "private (" ^ string_of_int x ^ ")"

let pp fmt t = Format.pp_print_string fmt (to_string t)
let print t = print_string (to_string t)
