module type RandomGen = sig
  type t
  val random : unit -> t
end

module RandomStd : RandomGen with type t = float = struct
  type t = float
  let random () = Stdlib.Random.float 1.0
end