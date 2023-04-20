type t = int

let to_bool n = n <> 0
let of_bool = Bool.to_int
let make n _ = n
let arbitrary _ = 0
let reveal_int n = n
let mux s m n = if to_bool s then m else n
let add = ( + )
let sub = ( - )
let mul = ( * )
let div = ( / )
let eq m n = m = n |> of_bool
let le m n = m <= n |> of_bool
let band m n = (to_bool m && to_bool n) |> of_bool
let bor m n = (to_bool m || to_bool n) |> of_bool
let bnot n = (not (to_bool n)) |> of_bool
