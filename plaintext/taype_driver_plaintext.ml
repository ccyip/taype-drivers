open Taype_driver

module OInt : OInt = struct
  type t = int

  let to_bool n = n <> 0
  let of_bool = Bool.to_int
  let count : int ref = ref 0

  let setup_driver _ _ _ = function
    | PublicP -> ()
    | PrivateP _ -> raise Unknown_party

  let finalize_driver () = ()
  let collect_stat () = count := 0
  let report_stat () = !count
  let make _ n = n
  let reveal_int n = n
  let reveal_bool n = to_bool n

  let mux s m n =
    count := !count + 1;
    if to_bool s then m else n

  let add = ( + )
  let sub = ( - )
  let mul = ( * )
  let div = ( / )
  let eq m n = m = n |> of_bool
  let le m n = m <= n |> of_bool
  let band m n = (to_bool m && to_bool n) |> of_bool
  let bor m n = (to_bool m || to_bool n) |> of_bool
  let bnot n = (not (to_bool n)) |> of_bool
end

module Driver : S = Make (OInt)
