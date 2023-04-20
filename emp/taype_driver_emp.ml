open Taype_driver
module F = C.Functions

module OInt : OInt = struct
  type t = C.Types.t

  let stamp : float ref = ref 0.0
  let check_party_idx p = if p < 1 || p > 2 then raise Unknown_party

  let setup_driver verbose addr port party =
    let p =
      match party with
      | Party.Public -> raise Unknown_party
      | Party.Private p ->
          check_party_idx p;
          p
    in
    let addr = if p = 1 then None else Some addr in
    F.setup_driver addr port p (not verbose)

  let finalize_driver = F.finalize_driver
  let collect_stat () = stamp := Unix.gettimeofday ()

  let report_stat () =
    let now = Unix.gettimeofday () in
    (* Convert to microseconds. *)
    (now -. !stamp) *. 1000000.0 |> Int.of_float

  let finalise p =
    Gc.finalise F.obliv_int_destroy p;
    p

  let int_of_party = function
    | Party.Public -> 0
    | Party.Private p ->
        check_party_idx p;
        p

  let make p n = F.obliv_int_new n (int_of_party p) |> finalise
  let arbitrary p = make p 0
  let reveal_int = F.obliv_int_reveal
  let mux s m n = F.obliv_int_mux s m n |> finalise
  let add m n = F.obliv_int_add m n |> finalise
  let sub m n = F.obliv_int_sub m n |> finalise
  let mul m n = F.obliv_int_mul m n |> finalise
  let div m n = F.obliv_int_div m n |> finalise
  let eq m n = F.obliv_int_eq m n |> finalise
  let le m n = F.obliv_int_le m n |> finalise
  let band m n = F.obliv_bool_and m n |> finalise
  let bor m n = F.obliv_bool_or m n |> finalise
  let bnot m = F.obliv_bool_not m |> finalise
end

module Driver : S = Make (OInt)
