module type Real = sig
  type t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val of_float : float -> t
  val to_float : t -> float
end

module FloatReal : Real = struct
  type t = float
  let zero = 0.0
  let one = 1.0
  let ( + ) x y = x +. y
  let ( - ) x y = x -. y
  let ( * ) x y = x *. y
  let ( / ) x y = x /. y
  let of_float x = x
  let to_float x = x
end