include module type of Intf

module Make_naive : OInt -> S
(** Create a naive driver from an oblivious integer module. The return module is
    as naive as possible, so that it serves as a reference implementation. While
    the performance of this module is not good, it is more likely to be
    correct. *)

module Make : OInt -> S
(** Create a driver from an oblivious integer module. This module aim to be
    effieicent. *)
