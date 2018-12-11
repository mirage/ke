[@@@warning "-37"]

module Peano = struct
  type zero = Zero
  type 'a succ = Succ

  type one = zero succ
  type two = zero succ succ
  type three = zero succ succ
end

type ('a, 'l) digit =
  | Zero : ('a, Peano.zero) digit
  | One : 'a -> ('a, Peano.one) digit
  | Two : 'a * 'a -> ('a, Peano.two) digit
  | Three : 'a * 'a * 'a -> ('a, Peano.three) digit

type 'a t =
  | Shallow : ('a, 'l) digit -> 'a t
  | Deep : { s : int
           ; f : ('a, 'f Peano.succ) digit
           ; m : ('a * 'a) t Lazy.t
           ; r : ('a, 'r Peano.succ) digit } -> 'a t

let empty = Shallow Zero

exception Empty

let _one x = Shallow (One x)
let _two x y = Shallow (Two (x, y))
let _three x y z = Shallow (Three (x, y, z))

let _deep s f m r =
  Deep { s; f; m; r; }

let is_empty : type a. a t -> bool = function
  | Shallow Zero -> true
  | Shallow _ | Deep _ -> false

let rec push : type a. a t -> a -> a t = fun q x -> match q with
  | Shallow Zero -> _one x
  | Shallow (One y) -> _two y x
  | Shallow (Two (y, z)) -> _three y z x
  | Shallow (Three (a, b, c)) -> _deep 4 (Two (a, b)) (Lazy.from_val empty) (Two (c, x))
  | Deep { s; f; m; r= One y } ->
    _deep (s + 1) f m (Two (y, x))
  | Deep { s; f; m; r= Two (y, z) } ->
    _deep (s + 1) f m (Three (y, z, x))
  | Deep { s; f; m= (lazy q'); r= Three (y, z, z') } ->
    _deep (s + 1)  f (lazy (push q' (y, z))) (Two (z', x))

let rec shift : type a. a t -> a * a t =
  fun q ->
    match q with
    | Shallow Zero -> raise Empty
    | Shallow (One x) -> x, empty
    | Shallow (Two (x, y)) -> x, _one y
    | Shallow (Three (x, y, z)) -> x, _two y z
    | Deep { s; f= One x; m= (lazy q'); r } ->
      if is_empty q'
      then (x, Shallow r)
      else
        let (y, z), q' = shift q' in
        (x, _deep (s - 1) (Two (y, z)) (Lazy.from_val q') r)
    | Deep { s; f= Two (x, y); m; r; } ->
      (x, _deep (s - 1) (One y) m r)
    | Deep { s; f= Three (x, y, z); m; r; } ->
      (x, _deep (s - 1) (Two (y, z)) m r)

let rec cons : type a. a t -> a -> a t = fun q x -> match q with
  | Shallow Zero -> _one x
  | Shallow (One y) -> _two x y
  | Shallow (Two (y, z)) -> _three x y z
  | Shallow (Three (y, z, z')) ->
    _deep 4 (Two (x, y)) (Lazy.from_val empty) (Two (z, z'))
  | Deep { s; f= One y; m; r; } ->
    _deep (s + 1) (Two (x, y)) m r
  | Deep { s; f= Two (y, z); m; r; } ->
    _deep (s + 1) (Three (x, y, z)) m r
  | Deep { s; f= Three (y, z, z'); m= (lazy q'); r; } ->
    _deep (s + 1) (Three (x, y, z)) (lazy (cons q' (z, z'))) r
