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

let rec pop_exn : type a. a t -> a * a t =
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
        let (y, z), q' = pop_exn q' in
        (x, _deep (s - 1) (Two (y, z)) (Lazy.from_val q') r)
    | Deep { s; f= Two (x, y); m; r; } ->
      (x, _deep (s - 1) (One y) m r)
    | Deep { s; f= Three (x, y, z); m; r; } ->
      (x, _deep (s - 1) (Two (y, z)) m r)

let peek_exn : type a. a t -> a =
  fun q ->
    match q with
    | Shallow Zero -> raise Empty
    | Shallow (One x) -> x
    | Shallow (Two (x, _)) -> x
    | Shallow (Three (x, _, _)) -> x
    | Deep { f = One x; _ } -> x
    | Deep { f = Two (x, _); _ } -> x
    | Deep { f = Three (x, _, _); _ } -> x

let pop q = try Some (pop_exn q) with Empty -> None
let peek q = try Some (peek_exn q) with Empty -> None

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

let iter : type a. (a -> unit) -> a t -> unit = fun f q ->
  let rec go : type a. (a -> unit) -> a t -> unit = fun f -> function
    | Shallow Zero -> ()
    | Shallow (One x) -> f x
    | Shallow (Two (x, y)) -> f x; f y
    | Shallow (Three (x, y, z)) -> f x; f y; f z
    | Deep { f= hd; m = lazy q; r= tl; _ } ->
      go f (Shallow hd); go (fun (x, y) -> f x; f y) q; go f (Shallow tl) in
  go f q

let fold : type acc x. (acc -> x -> acc) -> acc -> x t -> acc = fun f a q ->
  let rec go : type acc x. (acc -> x -> acc) -> acc -> x t -> acc = fun f a -> function
    | Shallow Zero -> a
    | Shallow (One x) -> f a x
    | Shallow (Two (x, y)) -> f (f a x) y
    | Shallow (Three (x, y, z)) -> f (f (f a x) y) z
    | Deep { f= hd; m= lazy q; r= tl; _ } ->
      let a = go f a (Shallow hd) in
      let a = go (fun a (x, y) -> f (f a x) y) a q in
      go f a (Shallow tl) in
  go f a q

let length = function
  | Deep { s; _ } -> s
  | Shallow Zero -> 0
  | Shallow (One _) -> 1
  | Shallow (Two _) -> 2
  | Shallow (Three _) -> 3
