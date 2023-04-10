open Ctypes

module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F
  open Types

  let setup_driver = foreign "setup_driver"
      (string_opt @-> int @-> int @-> bool @-> returning void)

  let finalize_driver = foreign "finalize_driver"
      (void @-> returning void)

  let obliv_int_new = foreign "obliv_int_new"
      (int @-> int @-> returning t)

  let obliv_int_destroy = foreign "obliv_int_destroy"
      (t @-> returning void)

  let obliv_int_reveal = foreign "obliv_int_reveal"
      (t @-> returning int)

  let obliv_bool_reveal = foreign "obliv_bool_reveal"
      (t @-> returning bool)

  let obliv_int_add = foreign "obliv_int_add"
      (t @-> t @-> returning t)

  let obliv_int_sub = foreign "obliv_int_sub"
      (t @-> t @-> returning t)

  let obliv_int_mul = foreign "obliv_int_mul"
      (t @-> t @-> returning t)

  let obliv_int_div = foreign "obliv_int_div"
      (t @-> t @-> returning t)

  let obliv_int_eq = foreign "obliv_int_eq"
      (t @-> t @-> returning t)

  let obliv_int_le = foreign "obliv_int_le"
      (t @-> t @-> returning t)

  let obliv_bool_not = foreign "obliv_bool_not"
      (t @-> returning t)

  let obliv_bool_and = foreign "obliv_bool_and"
      (t @-> t @-> returning t)

  let obliv_bool_or = foreign "obliv_bool_or"
      (t @-> t @-> returning t)

  let obliv_int_mux = foreign "obliv_int_mux"
      (t @-> t @-> t @-> returning t)

end
