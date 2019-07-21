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
  | Deep :
      { s: int
      ; f: ('a, 'f Peano.succ) digit
      ; m: ('a * 'a) t Lazy.t
      ; r: ('a, 'r Peano.succ) digit }
      -> 'a t

let empty = Shallow Zero

exception Empty

let _one x = Shallow (One x)
let _two x y = Shallow (Two (x, y))
let _three x y z = Shallow (Three (x, y, z))
let _deep s f m r = Deep {s; f; m; r}

let is_empty : type a. a t -> bool = function
  | Shallow Zero -> true
  | Shallow _ | Deep _ -> false

let rec push : type a. a t -> a -> a t =
 fun q x ->
  match q with
  | Shallow Zero -> _one x
  | Shallow (One y) -> _two y x
  | Shallow (Two (y, z)) -> _three y z x
  | Shallow (Three (a, b, c)) ->
      _deep 4 (Two (a, b)) (Lazy.from_val empty) (Two (c, x))
  | Deep {s; f; m; r= One y} -> _deep (s + 1) f m (Two (y, x))
  | Deep {s; f; m; r= Two (y, z)} -> _deep (s + 1) f m (Three (y, z, x))
  | Deep {s; f; m= (lazy q'); r= Three (y, z, z')} ->
      _deep (s + 1) f (lazy (push q' (y, z))) (Two (z', x))

let rec pop_exn : type a. a t -> a * a t =
 fun q ->
  match q with
  | Shallow Zero -> raise Empty
  | Shallow (One x) -> (x, empty)
  | Shallow (Two (x, y)) -> (x, _one y)
  | Shallow (Three (x, y, z)) -> (x, _two y z)
  | Deep {s; f= One x; m= (lazy q'); r} ->
      if is_empty q' then (x, Shallow r)
      else
        let (y, z), q' = pop_exn q' in
        (x, _deep (s - 1) (Two (y, z)) (Lazy.from_val q') r)
  | Deep {s; f= Two (x, y); m; r} -> (x, _deep (s - 1) (One y) m r)
  | Deep {s; f= Three (x, y, z); m; r} -> (x, _deep (s - 1) (Two (y, z)) m r)

let rec tail_exn : type a. a t -> a t * a =
  fun q -> match q with
    | Shallow Zero -> raise Empty
    | Shallow (One x) -> empty, x
    | Shallow (Two (x, y)) -> _one x, y
    | Shallow (Three (x, y, z)) -> _two x y, z
    | Deep { s; f; m= (lazy q'); r= One x } ->
      if is_empty q'
      then (Shallow f, x)
      else
        let q'', (y, z) = tail_exn q' in
        (_deep (s - 1) f (Lazy.from_val q'') (Two (y, z)), x)
    | Deep { s; f; m; r= Two (x, y); } ->
      (_deep (s - 1) f m (One x), y)
    | Deep { s; f; m; r= Three (x, y, z); } ->
      (_deep (s - 1) f m (Two (x, y)), z)

let peek_exn : type a. a t -> a =
 fun q ->
  match q with
  | Shallow Zero -> raise Empty
  | Shallow (One x) -> x
  | Shallow (Two (x, _)) -> x
  | Shallow (Three (x, _, _)) -> x
  | Deep {f= One x; _} -> x
  | Deep {f= Two (x, _); _} -> x
  | Deep {f= Three (x, _, _); _} -> x

let pop q = try Some (pop_exn q) with Empty -> None
let tail q = try Some (tail_exn q) with Empty -> None
let peek q = try Some (peek_exn q) with Empty -> None

let rec cons : type a. a t -> a -> a t =
 fun q x ->
  match q with
  | Shallow Zero -> _one x
  | Shallow (One y) -> _two x y
  | Shallow (Two (y, z)) -> _three x y z
  | Shallow (Three (y, z, z')) ->
      _deep 4 (Two (x, y)) (Lazy.from_val empty) (Two (z, z'))
  | Deep {s; f= One y; m; r} -> _deep (s + 1) (Two (x, y)) m r
  | Deep {s; f= Two (y, z); m; r} -> _deep (s + 1) (Three (x, y, z)) m r
  | Deep {s; f= Three (y, z, z'); m= (lazy q'); r} ->
      _deep (s + 1) (Three (x, y, z)) (lazy (cons q' (z, z'))) r

let iter : type a. (a -> unit) -> a t -> unit =
 fun f q ->
  let rec go : type a. (a -> unit) -> a t -> unit =
   fun f -> function
    | Shallow Zero -> ()
    | Shallow (One x) -> f x
    | Shallow (Two (x, y)) -> f x ; f y
    | Shallow (Three (x, y, z)) -> f x ; f y ; f z
    | Deep {f= hd; m= (lazy q); r= tl; _} ->
        go f (Shallow hd) ;
        go (fun (x, y) -> f x ; f y) q ;
        go f (Shallow tl)
  in
  go f q

let rev_iter : type a. (a -> unit) -> a t -> unit =
  fun f q ->
   let rec go : type a. (a -> unit) -> a t -> unit =
    fun f -> function
     | Shallow Zero -> ()
     | Shallow (One x) -> f x
     | Shallow (Two (y, x)) -> f x; f y
     | Shallow (Three (z, y, x)) -> f x ; f y ; f z
     | Deep {f= hd; m= (lazy q); r= tl; _} ->
         go f (Shallow tl) ;
         go (fun (y, x) -> f x; f y) q ;
         go f (Shallow hd)
   in
   go f q

let fold : type acc x. (acc -> x -> acc) -> acc -> x t -> acc =
 fun f a q ->
  let rec go : type acc x. (acc -> x -> acc) -> acc -> x t -> acc =
   fun f a -> function
    | Shallow Zero -> a
    | Shallow (One x) -> f a x
    | Shallow (Two (x, y)) -> f (f a x) y
    | Shallow (Three (x, y, z)) -> f (f (f a x) y) z
    | Deep {f= hd; m= (lazy q); r= tl; _} ->
        let a = go f a (Shallow hd) in
        let a = go (fun a (x, y) -> f (f a x) y) a q in
        go f a (Shallow tl)
  in
  go f a q

let length = function
  | Deep {s; _} -> s
  | Shallow Zero -> 0
  | Shallow (One _) -> 1
  | Shallow (Two _) -> 2
  | Shallow (Three _) -> 3

let pp ?sep pp_elt = Fmt.iter ?sep iter pp_elt
let dump pp_elt = Fmt.Dump.iter iter (Fmt.always "fke") pp_elt

module Weighted = struct
  type ('a, 'b) t =
    { r: int
    ; w: int
    ; c: int
    ; k: ('a, 'b) Bigarray_compat.kind
    ; v: ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t }

  exception Empty
  exception Full

  let[@inline always] mask t v = v land (t.c - 1)
  let[@inline always] empty t = t.r = t.w
  let[@inline always] size t = t.w - t.r
  let[@inline always] full t = size t = t.c
  let[@inline always] available t = t.c - (t.w - t.r)
  let is_empty t = (empty [@inlined]) t
  let length q = size q

  let[@inline always] to_power_of_two v =
    let res = ref (pred v) in
    res := !res lor (!res lsr 1) ;
    res := !res lor (!res lsr 2) ;
    res := !res lor (!res lsr 4) ;
    res := !res lor (!res lsr 8) ;
    res := !res lor (!res lsr 16) ;
    succ !res

  let[@inline always] is_power_of_two v = v <> 0 && v land (lnot v + 1) = v

  let create ?capacity kind =
    let capacity =
      match capacity with
      | None | Some 0 -> 1
      | Some n ->
          if n < 0 then Fmt.invalid_arg "Rke.Weighted.create"
          else to_power_of_two n
    in
    ( { r= 0
      ; w= 0
      ; c= capacity
      ; k= kind
      ; v= Bigarray_compat.Array1.create kind Bigarray_compat.c_layout capacity }
    , capacity )

  let copy t =
    let v = Bigarray_compat.Array1.create t.k Bigarray_compat.c_layout t.c in
    Bigarray_compat.Array1.blit t.v v ;
    {r= t.r; w= t.w; c= t.c; v; k= t.k}

  let from v =
    if not (is_power_of_two (Bigarray_compat.Array1.dim v)) then
      Fmt.invalid_arg "RBA.from" ;
    let c = Bigarray_compat.Array1.dim v in
    let k = Bigarray_compat.Array1.kind v in
    {r= 0; w= 0; c; k; v}

  let push_exn t v =
    if (full [@inlined]) t then raise Full ;
    Bigarray_compat.Array1.unsafe_set t.v ((mask [@inlined]) t t.w) v ;
    {t with w= t.w + 1}

  let push t v = try Some (push_exn t v) with Full -> None

  let cons_exn t v =
    if (full [@inlined]) t then raise Full ;
    let i = t.r - 1 in
    Bigarray_compat.Array1.unsafe_set t.v ((mask [@inlined]) t i) v ;
    {t with r= i}

  let cons t v = try Some (cons_exn t v) with Full -> None

  let pop_exn t =
    if (empty [@inlined]) t then raise Empty ;
    let r = Bigarray_compat.Array1.unsafe_get t.v ((mask [@inlined]) t t.r) in
    (r, {t with r= t.r + 1})

  let pop t = try Some (pop_exn t) with Empty -> None

  let peek_exn t =
    if (empty [@inlined]) t then raise Empty ;
    Bigarray_compat.Array1.unsafe_get t.v ((mask [@inlined]) t t.r)

  let peek t = try Some (peek_exn t) with Empty -> None

  module N = struct
    type ('a, 'b) bigarray = ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t
    type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit
    type 'a length = 'a -> int

    let push_exn t ~blit ~length ?(off = 0) ?len v =
      let len = match len with None -> length v - off | Some len -> len in
      if (available [@inlined]) t < len then raise Full ;
      let msk = (mask [@inlined]) t t.w in
      let pre = t.c - msk in
      let rst = len - pre in
      let ret =
        if rst > 0 then (
          blit v off t.v msk pre ;
          blit v (off + pre) t.v 0 rst ;
          [ Bigarray_compat.Array1.sub t.v ((mask [@inlined]) t t.w) pre
          ; Bigarray_compat.Array1.sub t.v 0 rst ] )
        else (
          blit v off t.v msk len ;
          [Bigarray_compat.Array1.sub t.v ((mask [@inlined]) t t.w) len] )
      in
      (ret, {t with w= t.w + len})

    let push t ~blit ~length ?off ?len v =
      try Some (push_exn t ~blit ~length ?off ?len v) with Full -> None

    let keep_exn t ~blit ~length ?(off = 0) ?len v =
      let len = match len with None -> length v | Some len -> len in
      if (size [@inlined]) t < len then raise Empty ;
      let msk = (mask [@inlined]) t t.r in
      let pre = t.c - msk in
      let rst = len - pre in
      if rst > 0 then (
        blit t.v msk v off pre ;
        blit t.v 0 v (off + pre) rst )
      else blit t.v msk v off len

    let keep t ~blit ~length ?off ?len v =
      try Some (keep_exn t ~blit ~length ?off ?len v) with Empty -> None

    let unsafe_shift t len = {t with r= t.r + len}

    let shift_exn t len =
      if (size [@inlined]) t < len then raise Empty ;
      unsafe_shift t len

    let shift t len = try Some (shift_exn t len) with Empty -> None
  end

  let iter f t =
    let idx = ref t.r in
    let max = t.w in
    while !idx <> max do
      f (Bigarray_compat.Array1.unsafe_get t.v ((mask [@inlined]) t !idx)) ;
      incr idx
    done

  let rev_iter f t =
    if t.r == t.w then ()
    else
      let idx = ref (pred t.w) in
      let min = t.r in
      while
        f (Bigarray_compat.Array1.unsafe_get t.v ((mask [@inlined]) t !idx)) ;
        !idx <> min
      do
        decr idx
      done

  let fold f a t =
    let a = ref a in
    iter (fun x -> a := f !a x) t ;
    !a

  let clear t = {t with r= 0; w= 0}
  let unsafe_bigarray {v; _} = v
  let pp ?sep pp_elt = Fmt.iter ?sep iter pp_elt
  let dump pp_elt = Fmt.Dump.iter iter (Fmt.always "fke:weighted") pp_elt
end
