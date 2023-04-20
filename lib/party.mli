(** This module defines the party type used to identify participants of
    oblivious computation. *)

type t =
  | Public
  | Private of int  (** A private party is indexed by a positive number. *)

val to_string : t -> string

val pp : Format.formatter -> t -> unit

val print : t -> unit
