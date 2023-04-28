open Intf
open Containers

module Smart_OInt (OInt : OInt0) = struct
  type t = Arb | Known of int | Obliv of OInt.t

  let obliv x = Obliv x

  (* A sound but incomplete equality check. *)
  let cheap_eq x y =
    match (x, y) with
    | Arb, Arb -> true
    | Known m, Known n -> m = n
    | Obliv m, Obliv n -> Equal.physical m n
    | _, _ -> false

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

module Smart_OArray (OInt : OInt0) = struct
  (** This module implements a smart oblivious array. It tries to minimize the
      expensive array operations such as creation and copying, based on the
      behavior of Taype programs. *)

  module Elem = Smart_OInt (OInt)

  type t = { len : int; mutable v : t_ }

  (** This type is well-formed if:

      - None of the branches of [Seq] is empty, i.e. the length of [a1] and [a2]
        in [Seq a1 a2] must not be [0].

      - The branches of [Seq] cannot both be [Arb]; they should be merged into
        one [Arb].

      - The branches of [Seq] cannot be consecutive [Slice]; they should be
        merged into one [Slice].

      Throughout this module, we maintain this well-formedness invariant.
      Whenever possible, we try to reuse existing [Slice] to minimize the cost
      of converting [t_] to [array]. *)
  and t_ = Arb | Seq of t * t | One of Elem.t | Slice of Elem.t array * int

  let length t = t.len
  let arbitrary len = { len; v = Arb }
  let single x = { len = 1; v = One x }

  (* Assume the length of input is exactly 1. *)
  let get { v; _ } =
    match v with
    | Arb -> Elem.Arb
    | One x -> x
    | Slice (a, k) -> a.(k)
    (* A [Seq] has length at least [2]. *)
    | Seq _ -> assert false

  let concat ({ len = len1; v = v1 } as t1) ({ len = len2; v = v2 } as t2) =
    if len1 = 0 then t2
    else if len2 = 0 then t1
    else
      let v =
        match (v1, v2) with
        | Arb, Arb -> Arb
        | Slice (a, k1), Slice (a', k2)
          when Equal.physical a a' && k2 = k1 + len1 ->
            v1
        | _, _ -> Seq (t1, t2)
      in
      { len = len1 + len2; v }

  (* Invariant: [Array.length a = k + len]. *)
  let rec fill a k ({ len; v } as t) =
    (match v with
    (* [a] should be pre-filled by [Elem.Arb]. *)
    | Arb -> ()
    | One x -> a.(k) <- x
    | Slice (a', k') -> Array.blit a' k' a k len
    | Seq (l, r) ->
        fill a k l;
        fill a (k + l.len) r);
    t.v <- Slice (a, k)

  let force ({ len; v } as t) =
    match v with
    | Slice (a, k) -> (a, k)
    | _ ->
        let a = Array.make len Elem.Arb in
        fill a 0 t;
        (a, 0)

  let of_array a =
    let a = Array.map Elem.obliv a in
    { len = Array.length a; v = Slice (a, 0) }

  let to_array t =
    let a, k = force t in
    Array.init t.len (fun i -> Elem.force a.(k + i))

  (* Assume the range is in-bound. *)
  let rec slice ({ len; v } as t) k n =
    if n = 0 then { len = 0; v = Arb }
    else if k = 0 && n = len then t
    else
      match v with
      | Arb -> { len = n; v = Arb }
      | Slice (a, k') -> { len = n; v = Slice (a, k + k') }
      | Seq (l, _) when k + n <= l.len -> slice l k n
      | Seq (l, r) when l.len <= k -> slice r (k - l.len) n
      | Seq (l, r) ->
          (* This branch should be rarely accessed. *)
          concat (slice l k (l.len - k)) (slice r 0 (k + n - l.len))
      | One _ -> assert false

  let mux_slow s t1 t2 =
    let len = t1.len in
    let a1, k1 = force t1 in
    let a2, k2 = force t2 in
    let a = Array.init len (fun i -> Elem.mux s a1.(k1 + i) a2.(k2 + i)) in
    { len; v = Slice (a, 0) }

  (* Assume [t0] is a singleton, and [t1] and [t2] have the same length. *)
  let rec mux t0 ({ v = v1; len } as t1) ({ v = v2; _ } as t2) =
    let s = get t0 in
    match s with
    | Elem.Arb -> t1
    | Elem.Known s -> if Bool.of_int s then t1 else t2
    | _ -> (
        if Equal.physical v1 v2 then t1
        else if len = 1 then
          let m = get t1 in
          let n = get t2 in
          let x = Elem.mux s m n in
          if Elem.cheap_eq x s then t0
          else if Elem.cheap_eq x m then t1
          else if Elem.cheap_eq x n then t2
          else { len; v = One x }
        else
          match (v1, v2) with
          | Arb, _ -> t2
          | _, Arb -> t1
          | Seq (l1, r1), Seq (l2, r2) when l1.len = l2.len && r1.len = r2.len
            ->
              let l = mux t0 l1 l2 in
              let r = mux t0 r1 r2 in
              if Equal.physical l l1 && Equal.physical r r1 then t1
              else if Equal.physical l l2 && Equal.physical r r2 then t2
              else { len; v = Seq (l, r) }
          | _ -> mux_slow s t1 t2)
end

module Make = Driver.Make (Smart_OArray)
