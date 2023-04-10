open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  type t = unit ptr

  let t = ptr void
end
