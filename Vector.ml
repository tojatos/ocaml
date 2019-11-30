
module type VECTOR =
sig
  type vect = {i: float; j: float; k: float}
  val create: float*float*float -> vect
  val scalarProduct: vect*vect -> float
  val vectorProduct: vect*vect-> vect
  val length: vect -> float
  val toString: vect -> string
end
;;

module Vector: VECTOR =
struct
  type vect = {i: float; j: float; k: float} 
  let create(a1, a2, a3) = {i = a1; j = a2; k = a3}
  let length {i=a1; j=a2; k=a3} = sqrt(a1*.a1 +. a2*.a2 +. a3*.a3)
  let scalarProduct({i = a1; j = a2; k = a3}, {i = b1; j = b2; k = b3}) = 
        a1*.b1 +. a2*.b2 +. a3*.b3
  let vectorProduct({i = a1; j = a2; k = a3}, {i = b1; j = b2; k = b3}) = 
        {i = a2*.b3 -. a3*.b2; j = a3*.b1 -. a1*.b3; k = a1*.b2 -. a2*.b1}

  let toString{ i = a1; j = a2; k = a3} = 
    "["^string_of_float a1 ^", "^string_of_float a2 ^", "^string_of_float a3 ^"]"
end
;;

Vector.length (Vector.create(1., 2., 2.));;

Vector.scalarProduct({Vector.i = 1.; j = 3.; k = -5.}, {Vector.i = 4.; j = -3.; k = -1.});;

let a = Vector.create(2., 0., 1.);;
let b = Vector.create(1., -1., 3.);;
let axb = Vector.vectorProduct(a, b);;
Vector.toString axb;;

