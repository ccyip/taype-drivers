open Intf

module OArray (OInt : OInt0) = struct
  module Elem = OInt

  type t = Elem.t array

  let arbitrary = function
    | 0 -> [||]
    | n -> Elem.arbitrary Party.Public |> Array.make n

  let single x = [| x |]
  let hd a = a.(0)
  let length = Array.length
  let concat = Array.append
  let slice = Array.sub

  let mux a0 a1 a2 =
    let b = a0.(0) in
    Array.map2 (Elem.mux b) a1 a2

  let to_array a = a
  let of_array a = a
end

module Make = Driver.Make (OArray)
