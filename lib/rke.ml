type t =
  { mutable r: int
  ; mutable w: int
  ; mutable c: int
  ; mutable v: Bigstringaf.t }

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

let create ?capacity () =
  let capacity = match capacity with
    | None | Some 0 -> 1
    | Some n ->
      if n < 0 then Fmt.invalid_arg "Rke.create"
      else to_power_of_two n in
  { r= 0; w= 0; c= capacity; v= Bigstringaf.create capacity }

let grow t want =
  let max : int -> int -> int = max in
  let c = to_power_of_two (max 1 (max want (size t))) in
  if c <> Bigstringaf.length t.v
  then begin
    let dst = Bigstringaf.create c in
    let sze = (size[@inlined]) t in
    let msk = (mask[@inlined]) t t.r in
    let pre = t.c - msk in
    let rst = sze - pre in

    if rst > 0 then begin
      Bigstringaf.unsafe_blit t.v ~src_off:msk dst ~dst_off:0 ~len:pre ;
      Bigstringaf.unsafe_blit t.v ~src_off:0 dst ~dst_off:pre ~len:rst ;
    end else Bigstringaf.unsafe_blit t.v ~src_off:msk dst ~dst_off:0 ~len:sze ;

    t.v <- dst ;
    t.w <- sze ;
    t.c <- c ;
    t.r <- 0 ;
  end

let push t v =
  if (full[@inlined]) t then grow t (2 * (size[@inlined]) t);
  Bigstringaf.unsafe_set t.v ((mask[@inlined]) t t.w) v ;
  t.w <- t.w + 1

let cons t v =
  if (full[@inlined]) t then grow t (2 * (size[@inlined]) t);
  let i = t.r - 1 in
  Bigstringaf.unsafe_set t.v ((mask[@inlined]) t i) v ;
  t.r <- i

let pop t =
  let r = Bigstringaf.unsafe_get t.v ((mask[@inlined]) t t.r) in
  t.r <- t.r + 1; r

module N = struct
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
