open Taype_driver

module OInt = struct
  include Plaintext_OInt0

  let count : int ref = ref 0

  let setup_driver _ _ _ = function
    | Party.Trusted -> ()
    | _ -> raise Unknown_party

  let finalize_driver () = ()
  let collect_stat () = count := 0
  let report_stat () = !count

  let mux s m n =
    count := !count + 1;
    mux s m n
end

module Driver = Make (OInt)
module Driver_naive = Make_naive (OInt)