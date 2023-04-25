open Intf
module Make (OInt : OInt) : S with type obliv_int = OInt.t
