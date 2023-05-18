open Intf
open Containers

type comparison = Eq | Lt | Gt | Unknown

module Smart_OInt (OInt : OInt0) = struct
  type t = Arb | Known of int | Obliv of OInt.t

  let obliv x = Obliv x

  (* Sound and incomplete comparison. *)
  let cmp x y =
    match (x, y) with
    | _, _ when Equal.physical x y -> Eq
    | Arb, _ -> Gt
    | _, Arb -> Lt
    | Known m, Known n when m = n -> Eq
    | Obliv m, Obliv n when Equal.physical m n -> Eq
    | _, _ -> Unknown

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
  let get t =
    match t.v with
    | Arb -> Elem.Arb
    | One x -> x
    | Slice (a, k) -> a.(k)
    (* A [Seq] has length at least [2]. *)
    | Seq _ -> assert false

  let concat t1 t2 =
    if t1.len = 0 then t2
    else if t2.len = 0 then t1
    else
      let v =
        match (t1.v, t2.v) with
        | Arb, Arb -> Arb
        | Slice (a, k1), Slice (a', k2)
          when Equal.physical a a' && k2 = k1 + t1.len ->
            t1.v
        | _, _ -> Seq (t1, t2)
      in
      { len = t1.len + t2.len; v }

  (* Invariant: [Array.length a = k + len]. *)
  let rec fill a k t =
    (match t.v with
    (* [a] should be pre-filled by [Elem.Arb]. *)
    | Arb -> ()
    | One x -> a.(k) <- x
    | Slice (a', k') -> Array.blit a' k' a k t.len
    | Seq (l, r) ->
        fill a k l;
        fill a (k + l.len) r);
    t.v <- Slice (a, k)

  let force t =
    match t.v with
    | Slice (a, k) -> (a, k)
    | _ ->
        let a = Array.make t.len Elem.Arb in
        fill a 0 t;
        (a, 0)

  let of_array a =
    let a = Array.map Elem.obliv a in
    { len = Array.length a; v = Slice (a, 0) }

  let to_array t =
    let a, k = force t in
    Array.init t.len (fun i -> Elem.force a.(k + i))

  (* Assume the range is in-bound. *)
  let rec slice t k n =
    if n = 0 then { len = 0; v = Arb }
    else if k = 0 && n = t.len then t
    else
      match t.v with
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

  (* Sound and incomplete comparison, in terms of "refinement". [t1] is bigger
     than [t2], if [t1] has more "arbitrary" than [t2] but they are the same
     otherwise. Assume [t1] and [t2] have the same length. *)
  let rec cmp t1 t2 =
    let join c1 c2 =
      match (c1, c2) with
      | Eq, c | c, Eq -> c
      | Lt, Lt -> Lt
      | Gt, Gt -> Gt
      | _, _ -> Unknown
    in
    let len = t1.len in
    match (t1.v, t2.v) with
    | _, _ when Equal.physical t1.v t2.v -> Eq
    | Arb, _ -> Gt
    | _, Arb -> Lt
    | _, _ when len = 1 -> Elem.cmp (get t1) (get t2)
    | Slice (a1, k1), Slice (a2, k2) when Equal.physical a1 a2 && k1 = k2 -> Eq
    | Seq (l, r), Slice (a, k) ->
        join
          (cmp l { len = l.len; v = Slice (a, k) })
          (cmp r { len = r.len; v = Slice (a, k + l.len) })
    | Slice (a, k), Seq (l, r) ->
        join
          (cmp { len = l.len; v = Slice (a, k) } l)
          (cmp { len = r.len; v = Slice (a, k + l.len) } r)
    | _, _ -> Unknown

  (* Assume [t0] is a singleton, and [t1] and [t2] have the same length. *)
  let rec mux t0 t1 t2 =
    let s = get t0 in
    match s with
    | Elem.Arb -> t1
    | Elem.Known s -> if Bool.of_int s then t1 else t2
    | _ -> (
        match cmp t1 t2 with
        | Lt -> t1
        | Gt -> t2
        | Eq -> ( match (t1.v, t2.v) with _, Slice _ -> t2 | _, _ -> t1)
        | Unknown -> (
            let len = t1.len in
            if len = 1 then
              let m = get t1 in
              let n = get t2 in
              let x = Elem.mux s m n in
              match Elem.cmp x s with Eq -> t0 | _ -> { len; v = One x }
            else
              match (t1.v, t2.v) with
              | Seq (l1, r1), Seq (l2, r2)
                when l1.len = l2.len && r1.len = r2.len ->
                  let l = mux t0 l1 l2 in
                  let r = mux t0 r1 r2 in
                  if Equal.physical l l1 && Equal.physical r r1 then t1
                  else if Equal.physical l l2 && Equal.physical r r2 then t2
                  else { len; v = Seq (l, r) }
              | _ -> mux_slow s t1 t2))
end

module Make = Driver.Make (Smart_OArray)
