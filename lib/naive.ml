open Intf

module Make (OInt : OInt) : S = struct
  type obliv_array = OInt.t Array.t

  let this_party = ref PublicP

  let setup_driver ?(verbose=false) addr port party =
    OInt.setup_driver verbose addr port party;
    this_party := party

  let finalize_driver = OInt.finalize_driver
  let collect_stat = OInt.collect_stat
  let report_stat = OInt.report_stat
  let obliv_array_length = Array.length
  let obliv_array_new n = OInt.make PublicP 0 |> Array.make n
  let obliv_array_concat = Array.append
  let obliv_array_slice = Array.sub

  let obliv_array_mux a0 a1 a2 =
    assert (Array.length a0 = 1 && Array.length a1 = Array.length a2);
    let b = a0.(0) in
    Array.map2 (OInt.mux b) a1 a2

  let obliv_int_s n = OInt.make PublicP n |> Array.make 1

  let obliv_binop op a1 a2 =
    assert (Array.length a1 = 1 && Array.length a2 = 1);
    op a1.(0) a2.(0) |> Array.make 1

  let obliv_int_add = obliv_binop OInt.add
  let obliv_int_sub = obliv_binop OInt.sub
  let obliv_int_mul = obliv_binop OInt.mul
  let obliv_int_div = obliv_binop OInt.div
  let obliv_int_le = obliv_binop OInt.le
  let obliv_int_eq = obliv_binop OInt.eq
  let obliv_bool_and = obliv_binop OInt.band
  let obliv_bool_or = obliv_binop OInt.bor

  let obliv_bool_not a =
    assert (Array.length a = 1);
    OInt.bnot a.(0) |> Array.make 1

  module Conceal = struct
    let obliv_array_new_for party n = Array.init n (fun _ -> OInt.make party 0)
    let obliv_array_new n = obliv_array_new_for !this_party n
    let obliv_int_s n = OInt.make !this_party n |> Array.make 1
    let obliv_bool_s b = obliv_int_s (Bool.to_int b)
  end

  module Reveal = struct
    let obliv_int_r a =
      assert (Array.length a = 1);
      OInt.reveal_int a.(0)

    let obliv_bool_r a =
      assert (Array.length a = 1);
      OInt.reveal_bool a.(0)
  end
end
