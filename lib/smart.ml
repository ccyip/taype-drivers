open Intf
open Containers

module Smart_OInt (OInt : OInt0) = struct
  type t = Arb | Known of int | Obliv of OInt.t

  let force = function
    | Arb -> OInt.arbitrary Party.Public
    | Known n -> OInt.make n Party.Public
    | Obliv x -> x

  let make n = function Party.Public -> Known n | p -> Obliv (OInt.make n p)
  let arbitrary = function Party.Public -> Arb | p -> Obliv (OInt.arbitrary p)

  let reveal_int = function
    | Arb -> 0
    | Known n -> n
    | Obliv x -> OInt.reveal_int x

  let mux s m n =
    match (s, m, n) with
    | Arb, _, _ -> m
    | Known s, _, _ -> if Bool.of_int s then m else n
    | Obliv _, Arb, _ -> n
    | Obliv _, _, Arb -> m
    | Obliv _, Known m, Known n when m = n -> Known m
    | Obliv s, Known 1, Known 0 -> Obliv s
    | Obliv s, Known 0, Known 1 -> Obliv (OInt.bnot s)
    | Obliv _, Obliv m, Obliv n when Equal.physical m n -> Obliv m
    | Obliv s, _, _ -> Obliv (OInt.mux s (force m) (force n))

  let binop op obliv_op m n =
    match (m, n) with
    | Arb, _ -> Arb
    | _, Arb -> Arb
    | Known m, Known n -> Known (op m n)
    | _, _ -> Obliv (obliv_op (force m) (force n))

  let add = binop Plaintext_OInt0.add OInt.add
  let sub = binop Plaintext_OInt0.sub OInt.sub
  let mul = binop Plaintext_OInt0.mul OInt.mul
  let div = binop Plaintext_OInt0.div OInt.div
  let eq = binop Plaintext_OInt0.eq OInt.eq
  let le = binop Plaintext_OInt0.le OInt.le
  let band = binop Plaintext_OInt0.band OInt.band
  let bor = binop Plaintext_OInt0.bor OInt.bor

  let bnot = function
    | Arb -> Arb
    | Known n -> Known (Plaintext_OInt0.bnot n)
    | Obliv x -> Obliv (OInt.bnot x)
end

module OArray (OInt : OInt0) = struct
  module Elem = Smart_OInt (OInt)

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

  let to_array a = Array.map Elem.force a
  let of_array a = Array.map (fun x -> Elem.Obliv x) a
end

module Make = Driver.Make (OArray)
