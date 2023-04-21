open Real
open FloatReal

type 'a prob = unit -> 'a
let prb (f : (unit -> 'a)) : 'a prob = f
let app (p : 'a prob) : 'a = p ()

let rec func : t -> t = fun x -> if x <= one then one else x * func (x - one)