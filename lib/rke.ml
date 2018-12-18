type ('a, 'b) t =
  { mutable r: int
  ; mutable w: int
  ; mutable c: int
  ; k: ('a, 'b) Bigarray.kind
  ; mutable v: ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t }

exception Empty

external ( = ) : 'a -> 'a -> bool = "%equal"
let ( = ) (a : int) b = a = b

let[@inline always] mask t v = v land (t.c - 1)
let[@inline always] empty t = t.r = t.w
let[@inline always] size t = t.w - t.r
let[@inline always] available t = t.c - (t.w - t.r)
let[@inline always] full t = size t = t.c

let[@inline always] to_power_of_two v =
  let res = ref (pred v) in
  res := !res lor (!res lsr 1) ;
  res := !res lor (!res lsr 2) ;
  res := !res lor (!res lsr 4) ;
  res := !res lor (!res lsr 8) ;
  res := !res lor (!res lsr 16) ;
  succ !res

let is_empty t = (empty[@inlined]) t

let create ?capacity kind =
  let capacity = match capacity with
    | None | Some 0 -> 1
    | Some n ->
      if n < 0 then Fmt.invalid_arg "Rke.create"
      else to_power_of_two n in
  { r= 0; w= 0; c= capacity; k= kind; v= Bigarray.Array1.create kind Bigarray.c_layout capacity }

let grow t want =
  let max : int -> int -> int = max in
  let c = to_power_of_two (max 1 (max want (size t))) in
  if c <> Bigarray.Array1.dim t.v
  then begin
    let dst = Bigarray.Array1.create t.k Bigarray.c_layout c in
    let sze = (size[@inlined]) t in
    let msk = (mask[@inlined]) t t.r in
    let pre = t.c - msk in
    let rst = sze - pre in

    if rst > 0 then begin
      Bigarray.Array1.(blit (sub t.v msk pre) (sub dst 0 pre)) ;
      Bigarray.Array1.(blit (sub t.v 0 rst) (sub dst pre rst)) ;
    end else Bigarray.Array1.(blit (sub t.v msk sze) (sub dst 0 sze)) ;

    t.v <- dst ;
    t.w <- sze ;
    t.c <- c ;
    t.r <- 0 ;
  end

let push t v =
  if (full[@inlined]) t then grow t (2 * (size[@inlined]) t);
  Bigarray.Array1.unsafe_set t.v ((mask[@inlined]) t t.w) v ;
  t.w <- t.w + 1

let cons t v =
  if (full[@inlined]) t then grow t (2 * (size[@inlined]) t);
  let i = t.r - 1 in
  Bigarray.Array1.unsafe_set t.v ((mask[@inlined]) t i) v ;
  t.r <- i

let pop_exn t =
  if (empty[@inlined]) t then raise Empty ;
  let r = Bigarray.Array1.unsafe_get t.v ((mask[@inlined]) t t.r) in
  t.r <- t.r + 1; r

let pop t = try Some (pop_exn t) with Empty -> None

let peek_exn t =
  if (empty[@inlined]) t then raise Empty ;
  Bigarray.Array1.unsafe_get t.v ((mask[@inlined]) t t.r)

let peek t = try Some (peek_exn t) with Empty -> None

module N = struct
  type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
  type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit
  type 'a length = 'a -> int

  let push t ~blit ~length ?(off = 0) ?len v =
    let len = match len with None -> length v - off | Some len -> len in
    if (available[@inlined]) t < len then grow t (len + (size[@inlined]) t) ;
    let msk = (mask[@inlined]) t t.w in
    let pre = t.c - msk in
    let rst = len - pre in

    if rst > 0 then begin
      blit v off t.v msk pre ;
      blit v (off + pre) t.v 0 rst ;
    end else blit v off t.v msk len

  let keep t ~blit ~length ?(off = 0) ?len v =
    let len = match len with None -> length v - off | Some len -> len in
    let msk = (mask[@inlined]) t t.w in
    let pre = t.c - msk in
    let rst = len - pre in

    if rst > 0 then begin
      blit t.v msk v off pre ;
      blit t.v 0 v (off + pre) rst ;
    end else blit t.v msk v off len

  let pop t len = t.r <- t.r + len
end

let iter f t =
  let idx = ref t.r in
  let max = t.w in

  while !idx <> max do
    f (Bigarray.Array1.unsafe_get t.v ((mask[@inlined]) t !idx)) ;
    incr idx
  done

let fold f a t =
  let a = ref a in
  iter (fun x -> a := f !a x) t ; !a
