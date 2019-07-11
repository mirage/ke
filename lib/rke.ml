type ('a, 'b) t =
  { mutable r: int
  ; mutable w: int
  ; mutable c: int
  ; k: ('a, 'b) Bigarray_compat.kind
  ; mutable v: ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t }

exception Empty

external ( = ) : 'a -> 'a -> bool = "%equal"

let ( = ) (a : int) b = a = b
let[@inline always] mask t v = v land (t.c - 1)
let[@inline always] empty t = t.r = t.w
let[@inline always] size t = t.w - t.r
let[@inline always] available t = t.c - (t.w - t.r)
let[@inline always] full t = size t = t.c
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

let is_empty t = (empty [@inlined]) t

let create ?capacity kind =
  let capacity =
    match capacity with
    | None | Some 0 -> 1
    | Some n ->
        if n < 0 then Fmt.invalid_arg "Rke.create" else to_power_of_two n
  in
  { r= 0
  ; w= 0
  ; c= capacity
  ; k= kind
  ; v= Bigarray_compat.Array1.create kind Bigarray_compat.c_layout capacity }

let capacity { c; _ } = c

let copy t =
  let v = Bigarray_compat.Array1.create t.k Bigarray_compat.c_layout t.c in
  Bigarray_compat.Array1.blit t.v v ;
  {r= t.r; w= t.w; c= t.c; v; k= t.k}

let grow t want =
  let max : int -> int -> int = max in
  let c = to_power_of_two (max 1 (max want (size t))) in
  if c <> Bigarray_compat.Array1.dim t.v then (
    let dst = Bigarray_compat.Array1.create t.k Bigarray_compat.c_layout c in
    let sze = (size [@inlined]) t in
    let msk = (mask [@inlined]) t t.r in
    let pre = t.c - msk in
    let rst = sze - pre in
    ( if rst > 0 then (
      Bigarray_compat.Array1.(blit (sub t.v msk pre) (sub dst 0 pre)) ;
      Bigarray_compat.Array1.(blit (sub t.v 0 rst) (sub dst pre rst)) )
    else Bigarray_compat.Array1.(blit (sub t.v msk sze) (sub dst 0 sze)) ) ;
    t.v <- dst ;
    t.w <- sze ;
    t.c <- c ;
    t.r <- 0 )

let push t v =
  if (full [@inlined]) t then grow t (2 * (size [@inlined]) t) ;
  Bigarray_compat.Array1.unsafe_set t.v ((mask [@inlined]) t t.w) v ;
  t.w <- t.w + 1

let cons t v =
  if (full [@inlined]) t then grow t (2 * (size [@inlined]) t) ;
  let i = t.r - 1 in
  Bigarray_compat.Array1.unsafe_set t.v ((mask [@inlined]) t i) v ;
  t.r <- i

let pop_exn t =
  if (empty [@inlined]) t then raise Empty ;
  let r = Bigarray_compat.Array1.unsafe_get t.v ((mask [@inlined]) t t.r) in
  t.r <- t.r + 1 ;
  r

let pop t = try Some (pop_exn t) with Empty -> None

let peek_exn t =
  if (empty [@inlined]) t then raise Empty ;
  Bigarray_compat.Array1.unsafe_get t.v ((mask [@inlined]) t t.r)

let peek t = try Some (peek_exn t) with Empty -> None

let blit src src_off dst dst_off len =
  let a = Bigarray_compat.Array1.sub src src_off len in
  let b = Bigarray_compat.Array1.sub dst dst_off len in
  Bigarray_compat.Array1.blit a b

let compress t =
  let len = length t in
  let msk = (mask [@inlined]) t t.r in
  let pre = t.c - msk in
  let rst = len - pre in
  if rst > 0 then
    if (available [@inlined]) t >= pre
    then (
      (* XXX(dinosaure): in this case, [pre + rst <= msk], so [blit] will not
         overlap bytes at the end of [t.v] (at offset [msk]). *)
      blit t.v 0 t.v pre rst ;
      blit t.v msk t.v 0 pre )
    else (
      let tmp = Bigarray_compat.Array1.create t.k Bigarray_compat.c_layout pre in
      blit t.v msk tmp 0 pre ;
      blit t.v 0 t.v pre rst ;
      blit tmp 0 t.v 0 pre )
  else blit t.v msk t.v 0 len ;
  t.r <- 0 ; t.w <- len

module N = struct
  type ('a, 'b) bigarray = ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t
  type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit
  type 'a length = 'a -> int

  let push t ~blit ~length ?(off = 0) ?len v =
    let len = match len with None -> length v - off | Some len -> len in
    if (available [@inlined]) t < len then grow t (len + (size [@inlined]) t) ;
    let msk = (mask [@inlined]) t t.w in
    let pre = t.c - msk in
    let rst = len - pre in
    if rst > 0 then (
      blit v off t.v msk pre ;
      blit v (off + pre) t.v 0 rst )
    else blit v off t.v msk len ;
    t.w <- t.w + len

  let keep_exn t ~blit ~length ?(off = 0) ?len v =
    let len = match len with None -> length v - off | Some len -> len in
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

  let peek t =
    let len = (size [@inlined]) t in
    if len == 0 then []
    else
      let msk = (mask [@inlined]) t t.r in
      let pre = t.c - msk in
      let rst = len - pre in
      if rst > 0 then
        [ Bigarray_compat.Array1.sub t.v msk pre
        ; Bigarray_compat.Array1.sub t.v 0 rst ]
      else [ Bigarray_compat.Array1.sub t.v msk len ]

  let unsafe_shift t len = t.r <- t.r + len

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

let pp ?sep pp_elt = Fmt.iter ?sep iter pp_elt
let dump pp_elt = Fmt.Dump.iter iter (Fmt.always "rke") pp_elt

let clear q =
  q.r <- 0 ;
  q.w <- 0

module Weighted = struct
  type ('a, 'b) t =
    { mutable r: int
    ; mutable w: int
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
    t.w <- t.w + 1

  let push t v = try Some (push_exn t v) with Full -> None

  let cons_exn t v =
    if (full [@inlined]) t then raise Full ;
    let i = t.r - 1 in
    Bigarray_compat.Array1.unsafe_set t.v ((mask [@inlined]) t i) v ;
    t.r <- i

  let cons t v = try Some (cons_exn t v) with Full -> None

  let pop_exn t =
    if (empty [@inlined]) t then raise Empty ;
    let r = Bigarray_compat.Array1.unsafe_get t.v ((mask [@inlined]) t t.r) in
    t.r <- t.r + 1 ;
    r

  let pop t = try Some (pop_exn t) with Empty -> None

  let peek_exn t =
    if (empty [@inlined]) t then raise Empty ;
    Bigarray_compat.Array1.unsafe_get t.v ((mask [@inlined]) t t.r)

  let peek t = try Some (peek_exn t) with Empty -> None

  let compress t =
    let len = length t in
    let msk = (mask [@inlined]) t t.r in
    let pre = t.c - msk in
    let rst = len - pre in
    if rst > 0 then
      if (available [@inlined]) t >= pre
      then (
        (* XXX(dinosaure): in this case, [pre + rst <= msk], so [blit] will not
          overlap bytes at the end of [t.v] (at offset [msk]). *)
        blit t.v 0 t.v pre rst ;
        blit t.v msk t.v 0 pre )
      else (
        let tmp = Bigarray_compat.Array1.create t.k Bigarray_compat.c_layout pre in
        blit t.v msk tmp 0 pre ;
        blit t.v 0 t.v pre rst ;
        blit tmp 0 t.v 0 pre )
    else blit t.v msk t.v 0 len ;
    t.r <- 0 ; t.w <- len

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
      t.w <- t.w + len ;
      ret

    let push t ~blit ~length ?off ?len v =
      try Some (push_exn t ~blit ~length ?off ?len v) with Full -> None

    let keep_exn t ~blit ~length ?(off = 0) ?len v =
      let len = match len with None -> length v - off | Some len -> len in
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

    let peek t =
      let len = (size [@inlined]) t in
      if len == 0 then []
      else
        let msk = (mask [@inlined]) t t.r in
        let pre = t.c - msk in
        let rst = len - pre in
        if rst > 0 then
          [ Bigarray_compat.Array1.sub t.v msk pre
          ; Bigarray_compat.Array1.sub t.v 0 rst ]
        else [ Bigarray_compat.Array1.sub t.v msk len ]

    let unsafe_shift t len = t.r <- t.r + len

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

  let pp ?sep pp_elt = Fmt.iter ?sep iter pp_elt
  let dump pp_elt = Fmt.Dump.iter iter (Fmt.always "rke:weighted") pp_elt

  let clear q =
    q.r <- 0 ;
    q.w <- 0
end
