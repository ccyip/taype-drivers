open Intf
open Containers

module Make0 (OArrayF : OArray) (OInt : OInt0) = struct
  module OArray = OArrayF (OInt)
  module Elem = OArray.Elem

  type obliv_array = OArray.t

  let obliv_array_length = OArray.length
  let obliv_array_new = OArray.arbitrary
  let obliv_array_concat = OArray.concat
  let obliv_array_slice = OArray.slice

  let obliv_array_mux a0 a1 a2 =
    assert (OArray.length a0 = 1 && OArray.length a1 = OArray.length a2);
    OArray.mux a0 a1 a2

  let obliv_int_s n = Elem.make n Party.Public |> OArray.single

  let binop op a1 a2 =
    assert (OArray.length a1 = 1 && OArray.length a2 = 1);
    op (OArray.get a1) (OArray.get a2) |> OArray.single

  let obliv_int_add = binop Elem.add
  let obliv_int_sub = binop Elem.sub
  let obliv_int_mul = binop Elem.mul
  let obliv_int_div = binop Elem.div
  let obliv_int_le = binop Elem.le
  let obliv_int_eq = binop Elem.eq
  let obliv_bool_and = binop Elem.band
  let obliv_bool_or = binop Elem.bor

  let obliv_bool_not a =
    assert (OArray.length a = 1);
    Elem.bnot (OArray.get a) |> OArray.single

  module Reveal = struct
    let obliv_int_r a =
      assert (OArray.length a = 1);
      Elem.reveal_int (OArray.get a)
  end
end

module Make (OArrayF : OArray) (OInt : OInt) = struct
  let this_party = ref Party.Public

  let setup_driver ?(verbose = false) addr port party =
    OInt.setup_driver verbose addr port party;
    this_party := party

  let finalize_driver = OInt.finalize_driver
  let collect_stat = OInt.collect_stat
  let report_stat = OInt.report_stat

  include Make0 (OArrayF) (OInt)
  module Plaintext = Make0 (OArrayF) (Plaintext_OInt0)

  let obliv_array_to_array = OArray.to_array

  module Conceal = struct
    let obliv_array_conceal a =
      let f x = OInt.make x !this_party in
      Array.map f (Plaintext.OArray.to_array a) |> OArray.of_array

    let obliv_array_conceal_with s x = obliv_array_conceal (s x)

    let obliv_array_new_for n party =
      Array.init n (fun _ -> OInt.arbitrary party) |> OArray.of_array

    let obliv_int_s = obliv_array_conceal_with Plaintext.obliv_int_s
    let obliv_bool_s b = obliv_int_s (Bool.to_int b)
    let obliv_int_s_for party = obliv_array_new_for 1 party
    let obliv_bool_s_for party = obliv_array_new_for 1 party
  end

  module Reveal = struct
    let obliv_array_reveal a =
      let f x = OInt.reveal_int x in
      Array.map f (OArray.to_array a) |> Plaintext.OArray.of_array

    let obliv_array_reveal_with r a = r (obliv_array_reveal a)
    let obliv_int_r = Reveal.obliv_int_r
    let obliv_bool_r a = Bool.of_int (obliv_int_r a)
  end
end
