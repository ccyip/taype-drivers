type party =
  | PublicP
  | PrivateP of int  (** A private party is indexed by a positive number. *)

exception Unknown_party

module type OInt = sig
  type t
  (** Oblivious integer, which also doubles as oblivious boolean *)

  val setup_driver : bool -> string -> int -> party -> unit
  (** [setup_driver verbose addr port party] initializes the driver context with
      IP address [addr] and [port], for [party]. *)

  val finalize_driver : unit -> unit
  (** Clean up the driver context. Must call this function after finishing the
      computation. *)

  val collect_stat : unit -> unit
  (** Start collecting performance statistics. *)

  val report_stat : unit -> int
  (** Return the collected performance statistics. The meaning of the return
      integer is specific to the driver. *)

  val make : party -> int -> t
  (** [make party n] creates an oblivious integer of value [n] for [party]. *)

  val reveal_int : t -> int
  (** [reveal_int x] reveals [x] as an integer. Improper use of this function
      can break the security guarantee. *)

  val reveal_bool : t -> bool
  (** [reveal_bool x] reveals [x] as a boolean. Improper use of this function
      can break the security guarantee. *)

  val mux : t -> t -> t -> t
  (** This constant-time multiplexer [mux s m n] selects a branch according to
      the oblivious condition [s]. It acts like [if s then m else n], but does
      not disclose the condition [s]. *)

  (** Oblivious integer arithmetic *)

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  (** Oblivious integer comparison

      They return oblivious boolean. *)

  val eq : t -> t -> t
  val le : t -> t -> t

  (** Oblivious boolean arithmetic *)

  val band : t -> t -> t
  val bor : t -> t -> t
  val bnot : t -> t
end

module type S = sig
  type t
  (** Oblivious array *)

  val setup_driver : ?verbose:bool -> string -> int -> party -> unit
  (** See {!OInt.setup_driver}. The value of [verbose] is [false] by default. *)

  val finalize_driver : unit -> unit
  (** See {!OInt.finalize_driver}. *)

  val collect_stat : unit -> unit
  (** See {!OInt.collect_stat}. *)

  val report_stat : unit -> int
  (** See {!OInt.report_stat}. *)

  val obliv_array_new : int -> t
  (** [obliv_array_new n] creates an oblivious array of length [n], filled with
      arbitrary values. *)

  val obliv_array_concat : t -> t -> t
  (** Concatenate two oblivious arrays. *)

  val obliv_array_slice : t -> int -> int -> t
  (** [obliv_array_slice a n s] takes [s] elements from offset [n] of oblivious
      array [a]. *)

  val obliv_array_mux : t -> t -> t -> t
  (** [obliv_array_mux a0 a1 a2] is similar to the oblivious integer multiplexer
      {!OInt.mux}, but acts pairwise on the given arrays. The condition [a0]
      must be a singleton array of an oblivious boolean, and [a1] and [a2] must
      have the same size. *)

  val obliv_int_s : int -> t
  (** Oblivious integer section *)

  (** Oblivious integer and boolean operations over oblivious arrays *)

  val obliv_int_add : t -> t -> t
  val obliv_int_sub : t -> t -> t
  val obliv_int_mul : t -> t -> t
  val obliv_int_div : t -> t -> t
  val obliv_int_le : t -> t -> t
  val obliv_int_eq : t -> t -> t
  val obliv_bool_not : t -> t
  val obliv_bool_and : t -> t -> t
  val obliv_bool_or : t -> t -> t

  module Conceal : sig
    (** This module includes functions that are used in the conceal phase. *)

    val obliv_array_new_for : party -> int -> t
    (** [obliv_array_new_for party n] creates an oblivious array of length [n]
        for [party]. This function is used by the party without the private data
        to help the data-owner conceal their private data. *)

    val obliv_array_new : int -> t
    val obliv_int_s : int -> t
    val obliv_bool_s : bool -> t
  end

  module Reveal : sig
    (** This module includes functions that are used in the reveal phase. Be
        careful, since misuse of the reveal functions can destroy the security
        guarantee. *)

    val obliv_int_r : t -> int
    (** Oblivious integer retraction *)

    val obliv_bool_r : t -> bool
    (** Oblivious boolean retraction *)
  end
end
