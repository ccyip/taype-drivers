(** This module includes the infrastructure for creating and using a Taype
    driver. *)

include module type of Intf

(** Create a naive driver from an oblivious integer module. The return module is
    as naive as possible, so that it serves as a reference implementation. While
    the performance of this module is not good, it is more likely to be correct. *)
module Make_naive (_ : OInt) : S

(** Create a driver from an oblivious integer module. This module aim to be
    effieicent. *)
module Make (_ : OInt) : S
