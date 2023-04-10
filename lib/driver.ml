(* open Intf *)

(* module Make (OInt : OInt) : S = struct *)
(*   type t *)

(*   let setup_driver ?(verbose=false) addr port party = *)
(*     OInt.setup_driver verbose addr port party *)
(*   let finalize_driver = OInt.finalize_driver *)
(*   let collect_stat = OInt.collect_stat *)
(*   let report_stat = OInt.report_stat *)

(*   let obliv_array_new = assert false *)

(*   let obliv_array_concat = assert false *)

(*   let obliv_array_slice = assert false *)

(*   let obliv_array_mux = assert false *)

(*   let obliv_int_s = assert false *)

(*   let obliv_int_add = assert false *)
(*   let obliv_int_sub = assert false *)
(*   let obliv_int_mul = assert false *)
(*   let obliv_int_div = assert false *)
(*   let obliv_int_le = assert false *)
(*   let obliv_int_eq = assert false *)
(*   let obliv_bool_not = assert false *)
(*   let obliv_bool_and = assert false *)
(*   let obliv_bool_or = assert false *)

(*   module Conceal = struct *)
(*     let obliv_array_new_for = assert false *)
(*     let obliv_array_new = assert false *)
(*     let obliv_int_s = assert false *)
(*   end *)

(*   module Reveal = struct *)
(*     let obliv_int_r = assert false *)
(*     let obliv_bool_r = assert false *)
(*   end *)
(* end *)

module Make = Naive.Make
