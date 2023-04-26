open Containers

type t = int

let make n _ = n
let arbitrary _ = 0
let reveal_int n = n
let mux s m n = if Bool.of_int s then m else n
let add = ( + )
let sub = ( - )
let mul = ( * )
let div = ( / )
let eq m n = m = n |> Bool.to_int
let le m n = m <= n |> Bool.to_int
let band m n = (Bool.of_int m && Bool.of_int n) |> Bool.to_int
let bor m n = (Bool.of_int m || Bool.of_int n) |> Bool.to_int
let bnot n = (not (Bool.of_int n)) |> Bool.to_int
